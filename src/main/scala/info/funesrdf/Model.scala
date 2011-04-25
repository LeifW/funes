package info.funesrdf

import org.scardf.jena.JenaGraphPlus

// Jena wrapper
import org.scardf.SetGraph
import com.hp.hpl.jena.rdf.model.ModelFactory  
import com.hp.hpl.jena.ontology.OntModelSpec   // Inferencing
//import com.hp.hpl.jena.tdb.TDBFactory          // DB Store
import com.hp.hpl.jena.ontology.OntDocumentManager

import com.hp.hpl.jena.reasoner.rulesys.Rule
import com.hp.hpl.jena.reasoner.rulesys.GenericRuleReasoner
import com.hp.hpl.jena.reasoner.ReasonerRegistry
import com.hp.hpl.jena.ontology.ProfileRegistry
import com.hp.hpl.jena.reasoner.rulesys.BuiltinRegistry
import com.hp.hpl.jena.vocabulary.{RDF, OWL=>jOWL}
import java.io.File
//import scala.collection.JavaConversions._
import scala.collection.JavaConversions.asScalaIterator
/*
data_store/
  schema.owl
  app.rules
  data_dump.ttl
 */
object Model extends JenaGraphPlus({
BuiltinRegistry.theRegistry register CountInstances

val base_dir = "app"
//val data_dir = "templates/_data_store"
//val rr = ReasonerRegistry.theRegistry
//val ruleUri = "http://jena.hpl.hp.com/2003/GenericRuleReasoner"

//val reasoner = rr.getFactory(ruleUri).create(null).asInstanceOf[GenericRuleReasoner]
//val rules = Rule.rulesFromURL("file:" + data_dir + "/owl.rules")
val rules = new File(base_dir, "app.rules")
rules.createNewFile // make file if it doesn't already exist.
val reasoner = new GenericRuleReasoner(Rule.rulesFromURL("file:" + rules))
reasoner.setOWLTranslation(true)
reasoner.setTransitiveClosureCaching(true)
//reasoner.setRules(rules)

val modelMaker = ModelFactory.createFileModelMaker(base_dir)
val bareModel = modelMaker.openModel("dataStore.ttl")
//val bareModel = ModelFactory.createFileModelMaker(base_dir).openModel("dump.ttl")
//val schema = ModelFactory.createDefaultModel.read("file:" + data_dir + "/schema.ttl", "TURTLE")
//val schema = ModelFactory.createDefaultModel.read("file:" + data_dir + "/schema.ttl", "TURTLE")
//val odm = OntDocumentManager.getInstance
//val schema_url = "file:" + data_dir + "/schema.ttl"
  /*
val schema = OntDocumentManager.getInstance.getOntology(
    "file:" + base_dir + "/schema.ttl",
    new OntModelSpec(
      modelMaker,
      null,
      null,
      ProfileRegistry.OWL_LANG
    )
)
*/
/*
  val s = ModelFactory.createNonreifyingModel.read("file:" + base_dir + "/schema.ttl", "TURTLE")
  val schema = ModelFactory.createOntologyModel(
    new OntModelSpec(
      modelMaker,
      null,
      null,
      ProfileRegistry.OWL_LANG
    ),
    s
  )
*/

//val schema = ModelFactory.createOntologyModel
//val schema = ModelFactory.createNonreifyingModel.read("file:" + data_dir + "/schema.ttl", "TURTLE")
//val schema_with_imports = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM, modelMaker, schema)
val schema_cache = new java.io.File(base_dir, ".schema_cache")
schema_cache.mkdir
val schema = ModelFactory.createOntologyModel(
  OntModelSpec.OWL_MEM,
  ModelFactory.createFileModelMaker(schema_cache.toString),
  modelMaker.openModel("schema.ttl")
)


/* This *should* work, but throws a freaky error: java.lang.VerifyError: (class: org/musicpath/Model$, method: <init> signature: ()V) Expecting to find object/array on stack
implicit def jena2ScalaIter[A](iter: com.hp.hpl.jena.util.iterator.ExtendedIterator[A]):Iterator[A] = JavaConversions.asScalaIterator(iter)
*/
// Clear out the instance data from the imported ontologies:
for {
  subModel <- asScalaIterator(schema.listSubModels)
  individual <- asScalaIterator(subModel.listIndividuals)
  if !individual.hasOntClass(jOWL.Class)
} individual.remove

/*
val imports = schema.listSubModels
while (imports.hasNext) {
  val individuals = imports.next.listIndividuals
  while(individuals.hasNext) {
    val individual = individuals.next
    if (!individual.hasOntClass(jOWL.Class)) {
      individual.remove
    } else {
      println(individual)
    }
  }
}
*/
//println(schema.getIndividual("http://moustaki.org/foaf.rdf#moustaki").getOntClass)
//val statements = schema.getBaseModel.listStatements(schema.createResource("http://moustaki.org/foaf.rdf#moustaki"),null, null)
//while (statements.hasNext) println(statements.next)
//val schema = ModelFactory.createNonreifyingModel.read("file:" + data_dir + "/schema.ttl", "TURTLE")
//val schema_with_imports = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM, modelMaker, schema)

//reasoner.bindSchema(schema_with_imports)

//val spec = new OntModelSpec(OntModelSpec.OWL_MEM)
//spec.setReasoner(reasoner.bindSchema(schema_with_imports))


//ModelFactory.createInfModel(reasoner, schema, bareModel)
//val m = ModelFactory.createOntologyModel(spec, modelMaker, bareModel)
//m.addSubModel(schema_with_imports)
//m
List(ModelFactory.createInfModel(reasoner, schema, bareModel), schema)
} : _*)
/*
val model = modelMaker.createModel("default")
val bareModel = modelMaker.openModel("default.nt")

val modelMaker = ModelFactory.createFileModelMaker("templates")
val bareModel = modelMaker.openModel("_data_store.ttl")

val model = modelMaker.createDefaultModel
val reasoner = new GenericRuleReasoner(Rule.rulesFromURL("file:templates/_app.rules"))
//reasoner.setMode(GenericRuleReasoner.HYBRID)
reasoner.setOWLTranslation(true)
//reasoner.setTransitiveClosureCaching(true)
ModelFactory.createInfModel(reasoner, ModelFactory.createDefaultModel)
ModelFactory.createInfModel(reasoner, model)
ModelFactory.createInfModel(reasoner, schemas, model)
*/
// Using Scardf's native N-Triple parser to load the model during testing.
//object Model extends JenaGraphPlus( ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM_MICRO_RULE_INF, TDBFactory.createModel("tdb_store.db")) )


// vim: set ts=4 sw=4 et:
