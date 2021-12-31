
object AppMain {
    def main(args : Array[String]) : Unit = {
        var l1 = List(1,2,3)
        var l2 = List(4,8,16)
        var l4 = List(4,5,6)
        var l3 = List(l1,l2,l4)

        //1
        println(ispeglaj(l3))

        //2
        println(saberiPodliste(l3))

        //3
        println
        println
        print(izbaciParne(l3))

        //4
        println
        println
        var l5 = List("Pera","Niko","Kerac")
        println(obrniStringove(l5))

        //5
        println
        println
        print(izbaciDeljiveSa3(l3))
    }

    //1)
    def ispeglaj(l : List[List[Any]]) : List[Any] = {
        l.reduce((x,y) => x ++ y)
    }

    //2)
    def saberiPodliste(l : List[List[Int]]) : List[Int] = {
        l.filter(x => x != Nil).map( podlista => podlista.reduce(_ + _))
    }

    //3)
    def izbaciParne(l : List[List[Int]]) : List[List[Int]] = {
       l.map( podlista => podlista.filter(x => x % 2 == 1))
            .filter( x => x != Nil)
    }

    //4)
    def obrniStringove(l : List[String]) : List[String] = {
        l.map(s => s.reverse)
    }

    //5)
    def izbaciDeljiveSa3(l : List[List[Int]]) : List[List[Int]] = {
        l.map(podlista => podlista.filter(x => x % 3 != 0)).filter( x => x != Nil)
    }

    //6)
    def kombinacija(l : List[List[Int]]) : List[List[Int]] = {
        izbaciDeljiveSa3(l).filter(podlista => podlista.length >= 5)
    }
}