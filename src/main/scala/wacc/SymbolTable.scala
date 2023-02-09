package wacc

case class SymbolTable(val scoper: Scoper) {
    import wacc.Types._

    var map = Map[(String, Int), SAType]()

    override def toString(): String = {
        map.toString()
    }

    def lookupVar(v: String): Option[SAType] = {
        val iter = scoper.getIterator()
        while (iter.hasNext) {
            val curr = iter.next()
            val key = (v, curr)
            if (map.contains(key)) return Some(map(key))
        }
        return None
    }

    def insertVar(v: String, t: SAType): Boolean = {
        val key = (v, scoper.getScope())
        if (map.contains(key)) return false
        map += (key -> t)
        return true
    }
}
