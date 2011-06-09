package org.scardf.jena
import org.scardf.{Node, UriRef, SubjectNode, RdfTriple, RDF}
import com.hp.hpl.jena.rdf.model.Model

class JenaGraphPlus(bindings:Map[String,String], models:Model*) extends JenaGraph(models.head) {
    def this(models:Model*) = this(Map.empty:Map[String,String], models:_*)
    def resourcesWithProperty(p:UriRef, o:Node) = new JenaResIterator( m.listResourcesWithProperty(property(p), rdfnode(o)) )
    def instancesOf(o:UriRef) = resourcesWithProperty(RDF.Type, o)

    def contains( s: SubjectNode, p:UriRef ) = m.contains( resource( s ), property( p ) )

    def remove( t: RdfTriple ) = m remove statement( t )

    def binding(elems: (String, String)*) = new JenaGraphPlus(Map(elems:_*), models:_*)

    def close = models.foreach(_.close)

    def read(file:String) = m.read("file:"+file, "TURTLE")
}


// vim: set ts=4 sw=4 et:
