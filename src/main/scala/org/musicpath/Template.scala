package org.musicpath

import org.scardf.{NodeConverter, SubjectNode, Property, GraphNode, UriRef, TypedLiteral, PlainLiteral, XSD, having}
import NodeConverter.{asGraphNode, asLexic}
import scala.xml.{NodeSeq, Node, Elem, Text, NamespaceBinding, MetaData, UnprefixedAttribute}
import java.net.URI

/*
  Welcome to Linked Data.
  You are at Node 0.
  Use "rel" attribute to traverse to a new subject (node).
  Use "property" to list a property of the current subject.
  Go!
 */

//class Template(pageUri:URI, includes:Map[String, Elem])
object Template {
  //var pageUri:URI
  object A { // convenience methods for retrieving the various RDFa attributes
    def curie(attrVal:String, scope:NamespaceBinding) = attrVal.split(':') match {
      case Array(prefix, local) => Some( UriRef( scope.getURI(prefix) + local) )
      case other => None  // If it's supposed to be a CURIE and isn't, ignore it, as per the spec.
    }
    def attr2Uri(attr:String, e:Elem) = e.attribute(attr).map(a=> UriRef(a.text) )
    def attr2Curie(attr:String, e:Elem) = e.attribute(attr).flatMap(a=> curie(a.text, e.scope))
    def attr2UriOrCurie(attr:String, e:Elem) = e.attribute(attr).flatMap { _.text.toList match {
        case '['::rest => curie( rest.takeWhile(_ != ']').mkString, e.scope )
        case other => Some( UriRef( other.mkString ) )
      }
    }
    def about(e:Elem) = attr2UriOrCurie("about", e)
    def resource(e:Elem) = attr2UriOrCurie("resource", e)
    def property(e:Elem) = attr2Curie("property", e)
    def rel(e:Elem) = attr2Curie("rel", e)
    def rev(e:Elem) = attr2Curie("rev", e)
    def src(e:Elem) = attr2Uri("src", e)
    def href(e:Elem) = attr2Uri("href", e)
    def templateValues(e:Elem) = e.attribute("template-values").map(_.text.split(' '))
    /*
    def values(e:Elem) = (kids:NodeSeq) => e.attribute("values") match {
        case Some(attr) => {
            values = attr.text.split(' ')
            kids.map({
                case Text(s) => s.format(values : _*)
                case other => other
            })
        }
        case None = kids 
    }
    */
                
  }

//def maybe[A,B](default:B, option:Option[A], f:A=>B) = option.map(f).getOrElse(default)

    def propertize(e:Elem)(subject:GraphNode):NodeSeq = {
        val processedChildren = e.child flatMap processLinks(subject)
        A.property(e) match {
              // Make as many copies of this Element as their are values for that predicate.
            case Some(predicate) => subject/predicate flatMap {obj =>
                val (text, dt) = datatype(obj)
                e.copy(attributes = datatypeAttr(dt, e.attributes), child = Text(text) +: processedChildren)
            } toSeq  // The many still needs to be of type Seq
            case None => {
                val templatedChildren = A.templateValues(e) match {
                    case Some(properties) =>  {
                        // TODO: invalid CURIEs get dropped off at this point.
                        val values = properties.map( A.curie(_, e.scope).get ).map(subject/_ map toScalaType head)
                        processedChildren.map({
                            case Text(s) => Text( s.format(values : _*) )
                            case other => other
                        })
                    }
                    case None => processedChildren
                }
                e.copy(child=templatedChildren, attributes = e.attributes.remove("template-values"))
                //e.copy( child = sprintf(e, subject, processedChildren) )
            }
        }
    }

    def toScalaType(l:org.scardf.Node) = l match {
        case TypedLiteral(string, XSD.string) => string
        case TypedLiteral(string, XSD.integer) => string.toInt
        case PlainLiteral(string, _) => string
        case UriRef(str) => str
    }


    def datatype(l:org.scardf.Node):(String, String) = l match {
        case TypedLiteral(string, XSD.string) => (string, "xs:string")
        case TypedLiteral(string, XSD.integer) => (string, "xs:integer")
        case PlainLiteral(string, _) => (string, "")
    }


    def datatypeAttr(datatype:String, attr:MetaData):MetaData = {
        if (datatype == "")
            attr
        else
            new UnprefixedAttribute("datatype", datatype, attr)
    }


    private def realizeLink(e:Elem, attr:String)(subject:GraphNode):Elem = {
        // If this element didn't come with a linky-type attribute, let's make one called say, "about"
        // If it did come with one, take it off, we're replacing it with the uri of the current subject.
        val (attrName, attrs) = if (attr == "")
            ("about", e.attributes)
        else
          (attr, e.attributes.remove(attr))
        // TODO: The cast won't work if there's blank nodes.
        val link = new UnprefixedAttribute(attrName, subject.node.asInstanceOf[UriRef].uri, attrs)
        // Templating of properties is done a bit different on elements with a resource link, thus is is handled directly here.
        // If the property has a value,, put it in there as a text node.  If not, delete the property attribute.
        // More than one value is a warning (put that property on a child element!).
        val processedChildren = e.child flatMap processLinks(subject)
        val (contents, attributes) = A.property(e) match {
            case Some(predicate) => subject/predicate map datatype match {
                case List() => (processedChildren, link.remove("property"))
                case List((text, dt)) => (Text(text) +: processedChildren, datatypeAttr(dt, link))
                case (text, dt)::_ => {
                  println("More than one "+predicate.uri+" found for "+subject.node.toString+" on element "+e.label+", ignoring rest.")
                  (Text(text) +: processedChildren, datatypeAttr(dt, link))
                }
                // I guess it wouldn't hurt to show the rest of the values found.
                // I just didn't want to have no values remove the element that has the link
          }
            // TODO: sprintf can happen here
            case None=> (processedChildren, link)
        }
        e.copy(attributes = attributes, child = contents )
    }

   // templateSingle vs. templateMany
   // templateyFn(subject, children, attributes, scope) => (children, attributes)

   // template(subject, elem, children)
   // template(subject, elem, children, attributes)
    private def copyTilLink(node:Node)(subject:GraphNode):Node = node match {
      case e:Elem => {
        val atts = e.attributes.map(_.key).toSet
        // Get the name of a linky attribute, if there is one.
        List("resource", "href", "src", "about").dropWhile(!atts.contains(_)).headOption match {
        //getLink(e) match {
          case Some(ref) => realizeLink(e, ref)(subject)
          case None => if ( List("property", "rel", "rev", "typeof") exists atts.contains  )
                          // RDFa blank node creation rule!  There should be a node here as the target of
                          // the rel and the subject of whatever property we found, so let's make one.
                          // It's in the spec, section 6.1.1.5.2
                          realizeLink(e, "")(subject)
                       else
                          // Recurse, carry on, keep looking.
                          // TODO: sprintf can happen here
                          e.copy( child = e.child.map(copyTilLink(_)(subject)) )
        }
      }
      case other=> other
    }

    def revs(subject:GraphNode, property:UriRef):Iterable[GraphNode] = {
        val graph = subject.graph
        //graph.resourcesWithProperty(property, subject.node).map(graph/_)  
        graph.triplesLike(SubjectNode, property, subject.node) map (graph/_.subj)
        //graph/-/having(property -> subject.node)/asGraphNode.iterable
    }

    // Performance issues:
    //def revs(subject:GraphNode, property:UriRef):Iterable[GraphNode] = subject.graph/-/having( property -> subject.node )/asGraphNode.iterable

    // Make two behaviours for this:
    // - One which additionally carries a new subject, doesn't look for rel attributes, and looks for a link attribute.
    // - The other, the default, is to just carry a subject, and be on the lookout for rel attributes.
    // We didn't have a rel when we came here:
    def processLinks(subject:GraphNode)(node:Node):NodeSeq = node match {
        case e:Elem => {
            val currentSubject = A.about(e) orElse A.src(e) map (subject.graph/_) getOrElse subject
            A.rel(e).map(currentSubject/_/asGraphNode.iterable) orElse A.rev(e).map(revs(currentSubject,_)) match {
            // By this point we should either have Some(List[New Subjects]) or None.
                case Some(newSubjects) => {
                    // Take the first link attribute name found:
                    Set("resource", "href") intersect e.attributes.map(_.key).toSet headOption match {
                          // If we ourselves are the target of the rel, copy ourselves as many times as need be.
                        case Some(ref) => newSubjects flatMap realizeLink(e, ref) toSeq
                           // Otherwise, take our first child that's an Element, and make as many copies as there are values for that rel,
                           // traversing down with that new subject to the first about/resource/etc attribute we can point it at.
                           // We want to call propertize on this case?
                           // Single-valued, at any rate.  Extract what's in realizeLink as a function.
                           // eg. e.copy( child = propOrSprintf( newSubjects.flatMap... )
                           // how about templateSingle(newSubjects...)
                        case None => e.copy( child = newSubjects.flatMap( copyTilLink( e.child.dropWhile(!_.isInstanceOf[Elem]).head ) ).toSeq) 
                    }
                }
                // TODO: Maybe there's a href or a resource attribute currently set on this element that makes a new subject for the kids?
                case None => propertize(e)(currentSubject)
            }
        }
        case other => other
        //case t:Text => t
    }
}

// vim: set ts=4 sw=4 et:
