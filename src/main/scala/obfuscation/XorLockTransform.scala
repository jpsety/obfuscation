// See README.md for license details.

package obfuscation

import firrtl.PrimOps.{Xor,Bits,Not,Cat,Head}
import firrtl.{CircuitState, LowForm, MALE, NodeKind, WRef}
import firrtl.ir._
import scala.collection.mutable.{Map, ListBuffer}
import scala.util.Random
import chisel3._
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform, annotate}
import chisel3.internal.InstanceId
import chisel3.testers.BasicTester
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.annotations.{SingleTargetAnnotation, ModuleName, Named}


/** FIRRTL annotation */
case class XorLockAnnotation(target: Named, value: String) extends SingleTargetAnnotation[Named] {
	  def duplicate(n: Named): XorLockAnnotation = this.copy(target = n)
}

/** ChiselAnnotation that corresponds to the above FIRRTL annotation */
case class XorLockChiselAnnotation(target: InstanceId, value: String) extends ChiselAnnotation with RunFirrtlTransform {
	def toFirrtl: XorLockAnnotation = XorLockAnnotation(target.toNamed, value)
	def transformClass: Class[XorLockTransform] = classOf[XorLockTransform]
}

object xorLock { // scalastyle:ignore object.name
	def apply(component: InstanceId, key: String): Unit = {
  		val anno = XorLockChiselAnnotation(component, key)
		annotate(anno)
	}
}

/**
  * Adds Keyed logic to a low FIRRTL circuit
  */
class XorLockTransform extends firrtl.Transform {

  	def inputForm: LowForm.type = LowForm
  	def outputForm: LowForm.type = LowForm

	//fix random seed
  	val rand = new Random(155)

	//variables passed through ast
	val stmtInds = ListBuffer[Int]()
	var stmtCount = 0
	var keyCount = 0
	var key = ""
	var keyLen = 0
	var moduleKeys = Map[String, String]()

  	def execute(state: CircuitState): CircuitState = {
   		val circuit = state.circuit

		//get annotations, contains key and module InstanceId
		state.annotations.map {
			case XorLockAnnotation(ModuleName(name,_), key) =>
				moduleKeys += (name -> key)
			case other => other
		}
		println(moduleKeys)

		// count all statements, this count is used to select a set of statements with uniform probability
		val modifiedModules = circuit.modules map keyModules

		//return modified circuit
		state.copy(circuit=circuit.copy(modules = modifiedModules))
	}

	//count all stmts in the design
	def keyModules(m: DefModule): DefModule = {
		m match {
			case mod @ firrtl.ir.Module(_,_,_,_) =>
				if (moduleKeys contains mod.name) {
					//get module key
					key = moduleKeys(mod.name)
					keyLen = key.length()
					println(key)

					//count statements in module
					stmtCount = 0
					mod mapStmt countStatements

					// randomly pick the statements to replace
					stmtInds.clear()
					stmtInds ++= rand.shuffle(List.range(0,stmtCount)).take(key.length()).sorted

					// add key port if the optimization has removed it
					val mod_io = if (mod.ports.exists(port => port.name == "io_obfKey") ) {
						println("io exists")
						mod 
					} else {
						println("io doesn't exist")
						new firrtl.ir.Module(mod.info, mod.name, mod.ports ++ Seq(Port(NoInfo,"io_obfKey",firrtl.ir.Input,UIntType(IntWidth(keyLen)))), mod.body)
					}

					//return modified module
					stmtCount = 0
					keyCount = 0
					mod_io mapStmt replaceStatements

				} else {mod}
			case other => other
		}
	}

  	//count stmts
    def countStatements(s: Statement): Statement = {
		s mapStmt countStatements
		s match {
		  case DefNode(_,_,_) =>
			stmtCount += 1
			s
		  case other => other
		}
  	}

  	//replace selected stmts
    def replaceStatements(s: Statement): Statement = {
    	val s_mod = s mapStmt replaceStatements
        s_mod match {
        	case node @ DefNode(_,_,_) =>
            	if (stmtInds.contains(stmtCount)) {
					// get key value
				  	val keyBit = key.substring(key.length()-1-keyCount,keyLen-keyCount)

				  	// select key input to connect
				  	val key_io_ref = WRef("io_obfKey",UIntType(IntWidth(keyLen)),NodeKind,MALE)
				  	val key_bit_op = DoPrim(Bits,Seq(key_io_ref),Seq(keyCount,keyCount),UIntType(IntWidth(1)))
				  	val key_bit_node = DefNode(NoInfo,s"KEY_BIT_NODE_${keyCount}",key_bit_op)
				  	val key_bit_ref = WRef(s"KEY_BIT_NODE_${keyCount}",UIntType(IntWidth(1)),NodeKind,MALE)

				  	// change the name of the original node
				  	val orig_node = DefNode(node.info,node.name + "_mod",node.value)
				  	val orig_ref = WRef(node.name + "_mod",node.value.tpe,NodeKind,MALE)

				  	node.value.tpe match {
					  	case GroundType(IntWidth(width)) if width > 1 => 
							var cat_width = 2
							val cat_nodes = ListBuffer[DefNode]()
							var key_bit_cat_ref = key_bit_ref
							while (cat_width<=width) {
								val key_bit_cat_op = DoPrim(Cat,Seq(key_bit_ref, key_bit_cat_ref),Seq(),UIntType(IntWidth(cat_width)))
								val key_bit_cat_node = DefNode(NoInfo,s"KEY_BIT_CAT_NODE_${keyCount}_${cat_width}",key_bit_cat_op)
								cat_nodes += key_bit_cat_node
								key_bit_cat_ref = WRef(s"KEY_BIT_CAT_NODE_${keyCount}_${cat_width}",UIntType(IntWidth(cat_width)),NodeKind,MALE)
								cat_width += 1
							}
					  
						  	if (keyBit == "1") {
								// connect inv
								val not_op = DoPrim(Not,Seq(orig_ref),Seq(),node.value.tpe)
								val not_node = DefNode(NoInfo,node.name+"_inv",not_op)
								val not_ref = WRef(node.name+"_inv",node.value.tpe,NodeKind,MALE)

								// connect xor
								val xor_op = DoPrim(Xor,Seq(key_bit_cat_ref,not_ref),Seq(),node.value.tpe)
								val xor_node = DefNode(NoInfo,node.name,xor_op)

								//increment inds and return new block statement
								stmtCount += 1
								keyCount += 1
								Block(Seq(key_bit_node) ++ cat_nodes ++ Seq(orig_node,not_node,xor_node))
						  
						  	} else {
								// connect xor
								val xor_op = DoPrim(Xor,Seq(key_bit_cat_ref,orig_ref),Seq(),node.value.tpe)
								val xor_node = DefNode(NoInfo,node.name,xor_op)
							
								//increment inds and return new block statement
								stmtCount += 1
								keyCount += 1
								Block(Seq(key_bit_node) ++ cat_nodes ++ Seq(orig_node,xor_node))
						  	}

					  	case GroundType(IntWidth(width)) if width == 1 => 
						  	if (keyBit == "1") {
								// connect inv
								val not_op = DoPrim(Not,Seq(orig_ref),Seq(),node.value.tpe)
								val not_node = DefNode(NoInfo,node.name+"_inv",not_op)
								val not_ref = WRef(node.name+"_inv",node.value.tpe,NodeKind,MALE)

								// connect xor
								val xor_op = DoPrim(Xor,Seq(key_bit_ref,not_ref),Seq(),node.value.tpe)
								val xor_node = DefNode(NoInfo,node.name,xor_op)

								//increment inds and return new block statement
								stmtCount += 1
								keyCount += 1
								Block(Seq(key_bit_node,orig_node,not_node,xor_node))
						  
						  	} else {
								// connect xor
								val xor_op = DoPrim(Xor,Seq(key_bit_ref,orig_ref),Seq(),node.value.tpe)
								val xor_node = DefNode(NoInfo,node.name,xor_op)
							
								//increment inds and return new block statement
								stmtCount += 1
								keyCount += 1
								Block(Seq(key_bit_node,orig_node,xor_node))
						  	}

				  		case _ => s_mod
					}

            	} else {
              		stmtCount += 1
              		s_mod
            	}
          	case other => other
        }
	}

}

