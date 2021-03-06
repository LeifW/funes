package info.funesrdf

import java.io.{File, FileWriter, FileOutputStream, StringWriter, FileReader, BufferedReader}
import scala.xml.{XML, Elem, Text}
import scala.io.Source
import scala.xml.parsing.ConstructingParser.{fromSource, fromFile}

import org.scalatra.ScalatraServlet  // Web framework
import org.scardf._                  // RDF lib

class Funes extends ScalatraServlet {
  val appRoot = "app"
  val serializer = new Serializator(NTriple)
  //val model = serializer.readFrom(new BufferedReader(new FileReader("dump.nt")))
  override def destroy = Model.close
  def loadXml(fileName:String):Elem = fromFile(new File(appRoot, fileName), true).document.docElem.asInstanceOf[Elem]

  val root = XML.load(appRoot+"/index.html")
  val hostname = root\"@hostname"  // Overrides to the hostname in the URL's of resources can go in a <html hostname="http://foo.com/"> attribute.
  //def template(templ:String, resource:String) = recurseNodes( model/UriRef(hostname+resource) )( XML.load("templates/"+templ) )
  /*
    Includes:
    XSPARQL: "_filename.xsparql"
    Scala: "objectname" where object is a function of type GraphNode=>Elem
   */
  val includes = Map(
    "xsparql" -> ((f:String, s:GraphNode)=> <loading>{f}</loading>),
    "scala" -> ((f:String, s:GraphNode)=> f match {
      case "upperCaseName" => Text(s/FOAF.givenname.v toUpperCase)
    } )
  )
  def template(templ:String, resource:String, lang:String) =
    Template(
      Model/UriRef(hostname+resource),
      includes + ("yield" -> {(s:String, _:GraphNode) =>
        // do some 733t haX0r shizit to copy the existing namespaces onto the child document.
        val source = Source.fromFile(appRoot+"/"+templ)
        val rootElem = source.takeWhile( c=> c != '>')
        val docWithNamespaces = Source.fromIterable((rootElem ++ Source.fromString(s + source.ch) ++ source).toIterable)
        //SequenceInputStream may be used if we want an InputStream (below) rather than an Iterator
        // This parser blows up if there's leading whitespace, whereas XML.load doesn't
        fromSource(docWithNamespaces, true).document.docElem }),
      lang
    )(loadXml("_layout.html"))

  get("/") {
    root
  }

/*
  get("/cull") {
    recurseNodes(Model/UriRef("http://musicpath.org/bands/cull"))(bandTemp)
  }
*/
 
//  before {
//    val lang = request.getLocale.getLanguage
//  }

  get("/:kind/") {
    val kind = params("kind")
    template(kind+"/index.html", kind+"/", request.getLocale.getLanguage)
  }

  get("/:kind/:id") {
    val kind = params("kind")
    val id = params("id")
    template(kind+"/view.html", kind+"/"+id, request.getLocale.getLanguage)
  }
}
