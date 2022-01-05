object AppMain { 
    def main(args : Array[String]) : Unit = {
        
        var list : List[Tuple2[Point,Int]] = List(
            ((3.0,4.0,5.0),1),
            ((2.0,4.0,3.0),1),
            ((1.0,4.0,2.0),2),
            ((6.0,4.0,5.0),2),
            ((1.2,4.0,2.3),2),
            ((4.5,3.0,5.0),1),
        )

        var n = 2;
        var t = (1.0,1.0,1.0);
        var k = 3;
        var destinationCluster = zad3_a(list,n,t,k)

        println(destinationCluster)
    }

    type Point = Tuple3[Double,Double,Double]
    
    def zad3_a(l : List[Tuple2[Point,Int]], n : Int, t : Point, k : Int) : Int = {
        
        var distances : List[Tuple2[Double,Int]] = l.map(x => (distanceTo(t)(x._1),x._2))

        distances = quickSort(distances) 
        //distance tacke T od tacaka iz L sa indexom kom klusteru pripadaju
        //koje su sortirane
        println(distances)

        var kFirst = List[Tuple2[Double,Int]](); //lista prvih k tacaka

        if(k > distances.length) {
            kFirst = distances.take(distances.length)   
        } else {
            kFirst = distances.take(k)
        }

        var counter : Array[Int] = Array.ofDim[Int](n) 
        //brojimo koliko k najblizih tacaka pripada kom klusteru
        
        for(el <- kFirst) {
            counter(el._2-1) += 1
        }

        var maxCount : Int = 0;

        for(el <- counter) {
            if(maxCount < el) {
                maxCount = el
            }
        }
        maxCount //oznaka kom klasteru ce pripadati nova tacka
        
    }

    def distanceTo(t : Point) (p : Point) : Double = {
        var dx = Math.pow(t._1 - p._1,2)
        var dy = Math.pow(t._2 - p._2,2)
        var dz = Math.pow(t._3 - p._3,2)

        return Math.sqrt(dx + dy + dz)
    }

    def quickSort(l : List[Tuple2[Double,Int]]) : List[Tuple2[Double,Int]] = {
        if(l == Nil || l.tail == Nil) return l

        var manji = manjiOdN(l.tail,l.head._1)
        var veci = veciOdN(l.tail,l.head._1)

        manji = quickSort(manji)
        veci = quickSort(veci)

        return manji ::: List(l.head) ::: veci
    }

    def manjiOdN(l : List[Tuple2[Double,Int]], n : Double) : List[Tuple2[Double,Int]] = {
        for(num <- l if num._1 <= n) yield num
    }

    def veciOdN(l : List[Tuple2[Double,Int]] , n : Double) : List[Tuple2[Double,Int]] = {
        for(num <- l if num._1 > n) yield num
    }
}