import scala.io.StdIn._

object AppMain {
    def main(args : Array[String]) : Unit = {

        // zad1()
        // zad2()
        // zad3()
        // zad4()
        // zad5()
        // zad6()
        zad7()
    }

    def zad1() : Unit = {
        var n1 : Int = readInt();

        var niz: Array[Int] = new Array[Int](n1)

        println("Unesi niz: ");

        for(i <- 0 until n1)
            niz(i) = readInt
        
        var rez = izbrojParne(niz)
        print(rez)

    }
    //1)
    def izbrojParne(niz : Array[Int]) : Int = {
        var c = 0;
        for(broj <- niz)
            if(broj % 2 == 0) c += 1
        c
    }


    def zad2() : Unit = {
        var n1 : Int = readInt();

        var niz: Array[String] = new Array[String](n1)

        println("Unesi niz: ");

        for(i <- 0 until n1)
            niz(i) = readLine
        
        print("Unesi granicu: ")
        var granica = readInt

        var l1 = duziOd(niz,granica)
        for(el <- l1) println(el)
    }
    //2)
    def duziOd(niz : Array[String], n : Int) : Array[String] = {
        for(el <- niz if el.length > n) yield el
    }

    //3) 
    def zad3() : Unit = {
        var niz : Array[Tuple2[Int,Int]] = izgenerisiParove(Array.range(0,11))
        for(el <- vratiOdgovarajuce(niz,3.5,4.5)) println(el)
    }

    def izgenerisiParove(brojevi : Array[Int]) : Array[Tuple2[Int,Int]] = {
        for(x <- brojevi; y <- brojevi ) yield (x,y)
    }

    def vratiOdgovarajuce(niz : Array[Tuple2[Int,Int]], a : Double, b : Double ) : Array[Tuple2[Int,Int]] = {
        for(el <- niz
            if (el._1 + el._2 ) / 2 >= a && (el._1 + el._2) / 2 <= b)
                yield el
    }

    //4)
    def zad4() : Unit = {
        println(skalarniProizvod(Array.range(1,10), Array.range(11,20)))
    }
    def skalarniProizvod(v1 : Array[Int], v2 : Array[Int]) : Int = {
        if(v1.length == v2.length) {
            var sum = 0;
            for(i <- 0 until v1.length)
                sum += (v1(i) * v2(i))
        sum
        } else {
            println("GRESKA - vektori nisu iste duzine!")
            -1
        }
    }

    //5)
    def zad5() : Unit = {
        var l = List(1,4,5,26,7,8,4,6,1)
        println(izbrojManjiOdN(l,6))
    }
    def izbrojManjiOdN(l : List[Int], n : Int) : Int = {
        l match {
            case Nil => 0
            case h :: t => 
                if (h < n) 
                    1 + izbrojManjiOdN(t,n)
                else
                    izbrojManjiOdN(t,n)
        }
    }

    //6)
    def zad6() : Unit = { 
        var l = List(1,4,5,23,7,8,4,6,1)
        for(el <- vratiProste(l)) println(el)
    }

    def vratiProste(l : List[Int]) : List[Int] = {
        l match { 
            case Nil => Nil
            case h :: t => 
            if(jeProst(h)) 
                h :: vratiProste(t)
            else
                vratiProste(t)
        }
    }
    def jeProst(n : Int) : Boolean = {
        for(i <- 2 to (n/2))
            if(n % i == 0) return false
        return true
    }

    //7)
    def zad7() : Unit = {
        var l = List(4,1,2,2,22,3,3,8,8,7,8)

        l = quickSort(l)
        for(el <- l) println(el)
    }

    def quickSort(l : List[Int]) : List[Int] = {
        if(l == Nil || l.tail == Nil) return l

        var manji = manjiOdN(l.tail,l.head)
        var veci = veciOdN(l.tail,l.head)

        manji = quickSort(manji)
        veci = quickSort(veci)

        return manji ::: List(l.head) ::: veci
    }

    def manjiOdN(l : List[Int], n : Int) : List[Int] = {
        for(num <- l if num <= n) yield num
    }

    def veciOdN(l : List[Int], n : Int) : List[Int] = {
        for(num <- l if num > n) yield num
    }
}