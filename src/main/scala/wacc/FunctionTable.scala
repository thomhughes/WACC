package wacc

import scala.collection.mutable.Map
import wacc.Types._

class FunctionTable {

    val map = Map[String, (Types.SAType, List[Types.SAType])]()

    def checkNotDuplicate(funcName: String) = map.contains(funcName)
  
    def insertFunction(funcName: String, params: (SAType, List[SAType])): Boolean = {
        if (checkNotDuplicate(funcName)) false
        else {
            map += (funcName -> params)
            true
        }
    }

    def getFunctionRet(funcName: String): Option[SAType] = {
        if (map.contains(funcName)) {
            val (retType, params) = map(funcName)
            return Some(retType)
        }
        None
    }

    def getFunctionParams(funcName: String): Option[List[SAType]] = {
        if (map.contains(funcName)) {
            val (retType, params) = map(funcName)
            return Some(params)
        }
        None
    }

    def containsFunction(funcName: String) = map.contains(funcName)

    override def toString(): String = map.toString()
}
