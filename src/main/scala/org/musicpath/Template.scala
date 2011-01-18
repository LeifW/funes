package org.musicpath

import org.scardf.{Node=>RdfNode, NodeConverter, SubjectNode, Property, GraphNode, UriRef, Blank, TypedLiteral, PlainLiteral, XSD, having}
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

object Template {
  def apply(page:GraphNode, includes:Map[String, (String, GraphNode)=>Node]) = new Template(page, includes)
}

class Template(page:GraphNode, includes:Map[String, (String, GraphNode)=>Node]) {
  val pageUri = new URI(page.node.asInstanceOf[UriRef].uri)
  //var pageUri:URI
  object A { // convenience methods for retrieving the various RDFa attributes
    def curie(attrVal:String, scope:NamespaceBinding) = attrVal.split(':') match {
      //case Array("_", local) => Some( Blank(local) )
      case Array(prefix, local) => Some( UriRef( scope.getURI(prefix) + local) )
      case other => None  // If it's supposed to be a CURIE and isn't, ignore it, as per the spec.
    }
    def attr2Uri(attr:String, e:Elem) = e.attribute(attr).flatMap(a=> if (a.text == "_") None else Some(UriRef(a.text)) )
    def attr2Curie(attr:String, e:Elem) = e.attribute(attr).flatMap(a=> curie(a.text, e.scope))
    def attr2UriOrCurie(attr:String, e:Elem) = e.attribute(attr).flatMap { _.text.toList match {
        case List('_') => None // This value means we want to fill it in with the current subject, not read from it.
        case '['::rest => curie( rest.takeWhile(_ != ']').mkString, e.scope )
        case other => Some( UriRef( other.mkString ) )
      }
    }
    def about(e:Elem) = attr2UriOrCurie("about", e)
    def resource(e:Elem) = attr2UriOrCurie("resource", e)
    def property(e:Elem) = attr2Curie("property", e)
    def typeof(e:Elem) = attr2Curie("typeof", e)
    def rel(e:Elem) = attr2Curie("rel", e)
    def rev(e:Elem) = attr2Curie("rev", e)
    def src(e:Elem) = attr2Uri("src", e)
    def href(e:Elem) = attr2Uri("href", e)
    def templateValues(e:Elem) = e.attribute("template-values").map( _.text.split(' ').map( curie(_, e.scope).get ) )
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

    def multiProperty(e:Elem)(subject:GraphNode):NodeSeq = {
        val processedChildren = e.child flatMap recurseNodes(subject)
        A.property(e) match {
              // Make as many copies of this Element as their are values for that predicate.
            case Some(predicate) => subject/predicate flatMap {value =>
                val (text, dt) = datatype(value)
                e.copy(attributes = datatypeAttr(dt, e.attributes), child = Text(text) +: processedChildren)
            } toSeq  // The many still needs to be of type Seq
            // No "property" attribute? Let's check for a "template-value" one.
            case None => A.templateValues(e) match {
                case Some(properties) =>  {
                    // TODO: invalid CURIEs get dropped off at this point.
                    // Take the first value for each property listed
                    // head may fail, might want to use headOption instead?
                    // If head fails, then "%s".format() will fail if not provided enough values.
                    val values = properties.map( (p)=> toScalaType(subject/p head) )
                    e.copy(
                      attributes = e.attributes.remove("template-values"),
                      child = processedChildren.map({
                        case Text(s) => Text( s.format(values : _*) )
                        case other => other
                      })
                    )
                }
                // No attributes (of interest) were found, do nothing but the recursion to kids.
                case None => e.copy(child =  processedChildren)
            }
        }
    }

    def toScalaType(l:RdfNode) = l match {
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


    def populateLink(existingAttributes:MetaData, attributeName:String)(subject:GraphNode):UnprefixedAttribute = {
      // If this element didn't come with a linky-type attribute, let's make one called say, "about"
      // If it did come with one, take it off, we're replacing it with the uri of the current subject.
      val (currentName, currentAttributes) =
        if (attributeName == "")
          ("about", existingAttributes)
        else
          (attributeName, existingAttributes.remove(attributeName))
          // Remove the "src" or whatever attrib here cuz we're gonna replace it with a filled-in version below.
      new UnprefixedAttribute(
        currentName,
        subject.node match {
          case u:UriRef => u.uri
          case b:Blank => b.rend
        },
        currentAttributes
      )
    }

    // Takes an Elem, and returns an Elem with some text node(s) filled in with whatever a "property" or "template-value" attribute said to lookup.
    // Also, if you don't supply the child elements as an argument, templating in general occurs in general recursively on the existing ones.
    def singleProperty(e:Elem)(subject:GraphNode):(MetaData, NodeSeq) = singleProperty(e, e.attributes)(subject)
    def singleProperty(e:Elem, attributes:MetaData)(subject:GraphNode):(MetaData, NodeSeq) = singleProperty(e, attributes, e.child)(subject)
    def singleProperty(e:Elem, attributes:MetaData, contents:NodeSeq)(subject:GraphNode):(MetaData, NodeSeq) = A.property(e) match {
        case Some(predicate) => subject/predicate map datatype match {
          case List() => (attributes.remove("property"), contents)
          case List((text, dt)) => (datatypeAttr(dt, attributes), Text(text) +: contents)
          case (text, dt)::_ => {
            println("More than one "+predicate.uri+" found for "+subject.node.toString+" on element "+e.label+", ignoring rest.")
            (datatypeAttr(dt, attributes), Text(text) +: contents)
          }
        // I guess it wouldn't hurt to show the rest of the values found.
        // I just didn't want to have no values remove the element that has the link
        }
        // TODO: sprintf can happen here
        case None=> (attributes, contents)
      }
/*
    private def populateLink(e:Elem, attr:String)(subject:GraphNode):Elem = {
        // If this element didn't come with a linky-type attribute, let's make one called say, "about"
        // If it did come with one, take it off, we're replacing it with the uri of the current subject.
        val (attrName, attrs) =
          if (attr == "")
            ("about", e.attributes)
          else
            (attr, e.attributes.remove(attr))
        // TODO: What do we want to do with blank nodes?
        val link = subject.node match {
            case u:UriRef => new UnprefixedAttribute(attrName, u.uri, attrs)
            case b:Blank => new UnprefixedAttribute(attrName, b.rend, attrs)
        }
        // Templating of properties is done a bit different on elements with a resource link, thus is is handled directly here.
        // If the property has a value,, put it in there as a text node.  If not, delete the property attribute.
        // More than one value is a warning (put that property on a child element!).
        val processedChildren = e.child flatMap recurseNodes(subject)
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
  */
   // templateSingle vs. templateMany
   // templateyFn(subject, children, attributes, scope) => (children, attributes)

   // template(subject, elem, children)
   // template(subject, elem, children, attributes)
    private def copyTilLink(node:Node)(subject:GraphNode):NodeSeq = node match {
      case Elem(null, "include", attributes, _, _ *) => <freakYield/> //ttributes.key match {
        //case "yield" => <yielded/>
      //
      // }
      case e:Elem => {
        val atts = e.attributes.map(_.key).toSet
        // Get the name of a linky attribute, if there is one.
        List("resource", "href", "src", "about").dropWhile(!atts.contains(_)).headOption match {
        //getLink(e) match {
          case Some(ref) => processLinks(subject, e.copy( attributes = populateLink(e.attributes, ref)(subject) ) )
          case None => if ( List("property", "rel", "rev", "typeof") exists atts.contains  )
                          // RDFa blank node creation rule!  There should be a node here as the target of
                          // the rel and the subject of whatever property we found, so let's make one.
                          // It's in the spec, section 6.1.1.5.2
                          processLinks(subject, e.copy( attributes = populateLink(e.attributes, "")(subject) ) )
                       else
                          // Recurse, carry on, keep looking.
                          // TODO: sprintf can happen here
                          e.copy( child = e.child.flatMap(copyTilLink(_)(subject)) )
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

    def processLinks(subject:GraphNode, e:Elem):NodeSeq = {
  val atts = e.attributes.map(_.key).toSet
  // Get the name of a linky attribute, if there is one.
  val attributes = Set("src", "about") intersect atts toList match {
    case ref::_  if (e.attribute(ref).get.text == "_") => populateLink(e.attributes, ref)(subject)
    case _ => e.attributes
  }
  // Maybe wanna fill in "_" blank about or src nodes here.
  // resource or href values will get replaced by rel if there is one, anyways.
  A.rel(e).map(subject/_/asGraphNode.iterable) orElse A.rev(e).map(revs(subject,_)) match {
  // By this point we should either have Some(List[New Subjects]) or None.
    case Some(newSubjects) => {
      // Take the first link attribute name found:
      val (proppedAttributes, contents) = singleProperty(e, attributes)(subject)
      Set("resource", "href") intersect atts headOption match {
      // If we ourselves are the target of the rel, copy ourselves as many times as need be.
        case Some(ref) => {
          // Property values on things like <a rel="foaf:knows" href="someoneelse.com" property="foaf:name"/> are still
          // in relation to the current subject, not the subjected traversed to by the rel.
          newSubjects.flatMap{ (s)=> e.copy( child = contents flatMap recurseNodes(s), attributes = populateLink(proppedAttributes, ref)(s) ) }.toSeq
        }
        // Otherwise, take our first child that's an Element, and make as many copies as there are values for that rel,
        // traversing down with that new subject to the first about/resource/etc attribute we can point it at.
        // We want to call propertize on this case?
        // Single-valued, at any rate.  Extract what's in realizeLink as a function.
        // eg. e.copy( child = propOrSprintf( newSubjects.flatMap... )
        // how about templateSingle(newSubjects...)
        // If you put a property or a template-value on an element with a rel, the text will appear before the child element.
        case None => e.copy( attributes = proppedAttributes, child = contents.dropWhile(!_.isInstanceOf[Text]).headOption.toSeq ++ newSubjects.flatMap( copyTilLink( e.child.dropWhile(!_.isInstanceOf[Elem]).headOption.getOrElse(error("Nothing for the rel to point to")) ) ).toSeq)
      }
    }
    case None => {
      // Not supporting schizoid references at this time.
      // How about only look for href or resource here -- the other two are taken care of up top.
      val (newAttributes, newSubject) = Set("resource", "href") intersect atts headOption match {
        case Some(refName) => e.attribute(refName).get.text match {
        // If the attribute value's blank, populate it with the current subject.
          case "_" => (populateLink(attributes, refName)(subject), subject)
          // Else, set the current subject to that value.
          case ref => refName match {
            case "resource" => (attributes, subject.graph/A.resource(e).get)
            case "href" => (attributes, subject.graph/A.href(e).get)
          }
        }
        case None => (attributes, subject)
      }
      multiProperty(e.copy( attributes = newAttributes ))(newSubject)
    }
  }
    }

    def recurseNodes(subject:GraphNode)(node:Node):NodeSeq = node match {
      case Elem(null, "include", attributes, _, _ *) => attributes.key match {
        case "yield" => recurseNodes(subject)(includes("yield")(node.scope.toString, subject))
        case other:String => includes(other)(attributes.value.text, subject)
    }
      case e:Elem => {
        val currentSubject = A.about(e) orElse A.src(e) map (subject.graph/_) getOrElse subject
        processLinks(currentSubject, e)
      }
      case other => other
        //case t:Text => t
    }

  def apply(node:Node):NodeSeq = recurseNodes(page)(node)
}

// vim: set ts=4 sw=4 et:
