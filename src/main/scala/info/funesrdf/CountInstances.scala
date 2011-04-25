package info.funesrdf

//package com.hp.hpl.jena.reasoner.rulesys.builtins
import com.hp.hpl.jena.reasoner.rulesys.builtins.BaseBuiltin
import com.hp.hpl.jena.reasoner.rulesys.{RuleContext, Util}
import com.hp.hpl.jena.graph.Node

/**
 * CountInstances(P, O, C) sets C to be the number of triples matching _, P, O.
 */
object CountInstances extends BaseBuiltin {
  /**
   * Return a name for this builtin, normally this will be the name of the
   * functor that will be used to invoke it.
   */
  val getName = "countInstances"

  /**
   * Return the expected number of arguments for this functor or 0 if the number is flexible.
   */
  override val getArgLength = 3

  /**
   * This method is invoked when the builtin is called in a rule body.
   * @param args the array of argument values for the builtin, this is an array
   * of Nodes, some of which may be Node_RuleVariables.
   * @param length the length of the argument list, may be less than the length of the args array
   * for some rule engines
   * @param context an execution context giving access to other relevant data
   * @return return true if the buildin predicate is deemed to have succeeded in
   * the current environment
   */
  override def bodyCall(args: Array[Node], length: Int, context: RuleContext): Boolean = {
    val p: Node = getArg(0, args, context)
    val o: Node = getArg(1, args, context)
    //val List(p, o) = List(0, 1).map(getArg(_, args, context))
    val nodes = context.find(null, p, o)
    var count = 0
    while (nodes.hasNext) {
      nodes.next
      count +=1
    }
    context.getEnv.bind(args(2), Util.makeIntNode(count))
  }
}
