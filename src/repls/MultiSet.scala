package repls

case class MultiSet[T] (multiplicity: Map[T, Int]) {

    def *(that: MultiSet[T]): MultiSet[T] = {
        val newMultiplicity = (for {
            key <- this.multiplicity.keySet.intersect(that.multiplicity.keySet)
            count = math.min(this.multiplicity(key), that.multiplicity(key))
            if count > 0}
        yield key -> count).toMap

        MultiSet(newMultiplicity)
    }

    def +(that: MultiSet[T]): MultiSet[T] = {
        val keys = this.multiplicity.keySet ++ that.multiplicity.keySet
        val newMultiplicity = (for {key <- keys
            count = this.multiplicity.getOrElse(key, 0) + that.multiplicity.getOrElse(key, 0)}
        yield key -> count).toMap

        MultiSet(newMultiplicity)
    }

    def -(that: MultiSet[T]): MultiSet[T] = {
        val newMultiplicity = (for {key <- this.multiplicity.keySet
            count = math.max(this.multiplicity.getOrElse(key, 0) - that.multiplicity.getOrElse(key, 0), 0)
            if count > 0}
        yield key -> count).toMap

        MultiSet(newMultiplicity)
    }

    def toSeq: Seq[T] = {
        multiplicity.flatMap{case (elem, count) => Seq.fill(count)(elem)}.toSeq
    }

    val MaxCountForDuplicatePrint = 5

    override def toString: String = {
        def elemToString(elem : T) : String = {
            val count = multiplicity(elem)
            if(count >= MaxCountForDuplicatePrint)
                elem.toString + " -> " + count.toString
            else Seq.fill(count)(elem).mkString(",")
        }
        val keyStringSet = multiplicity.keySet.map(elemToString)
        "{" + keyStringSet.toSeq.sorted.mkString(",") + "}"
    }
}

object MultiSet {
    def empty[T] : MultiSet[T] = MultiSet(Map[T,Int]())

    def apply[T](elements: Seq[T]): MultiSet[T] = {
        val multiplicityMap = elements.groupBy(identity).view.mapValues(_.size).toMap
        MultiSet(multiplicityMap)
    }
}
