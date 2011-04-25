
import info.funesrdf.Template
import org.scardf.{Turtle, UriRef}
import org.scardf.jena.JenaSerializator
import java.io.FileReader
import xml.XML

object template {
  def apply(template:String, triples:String) = Template(
    (new JenaSerializator(Turtle) readFrom new FileReader(triples))/UriRef(""), Map())(
    XML.loadFile(template)
  )
}
