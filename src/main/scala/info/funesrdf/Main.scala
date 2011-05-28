package info.funesrdf

import org.scardf.{Turtle, NTriple, UriRef, Serializator}
import org.scardf.jena.JenaSerializator
import java.io.{FileReader, OutputStreamWriter}
import xml.{XML, Node}
import xml.dtd.{DocType, PublicID}

object Main {
  def main( args:Array[String] ) {
    args match {
      case Array(template, graph) => 
        val out = new OutputStreamWriter(Console.out) 
      XML.write(
        out,
        //Template( (new JenaSerializator(Turtle) readFrom new FileReader(graph))/UriRef(""), Map.empty )( XML.loadFile(template) ).asInstanceOf[Node],
        Template( (new Serializator(NTriple) readFrom new FileReader(graph))/UriRef(""), Map.empty )( XML.loadFile(template) ).asInstanceOf[Node],
        "",
        false,
        null //DocType("html", PublicID("-//W3C//DTD XHTML Basic 1.1//EN", "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd"), List())
      )
      out.flush
      case other => println("Usage: funes <template.html> <rdf_graph.ttl>")
    }
  }
}
