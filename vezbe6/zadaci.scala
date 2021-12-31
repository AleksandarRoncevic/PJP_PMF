import scala.io.StdIn._

object AppMain {
    def main(args: Array[String]) : Unit = {
    //    var slovo : Char = 'a'
    //    println(tipKaraktera(slovo))

    // println(pomnoziSa(5,Array(1,2,3,4)))

    // var rez3 = kvadriraj(Array(1,2,3,4,5,6))
    // for(broj <- rez3)
    //     print(broj+" ")

    // stampajNPuta('$',6)

    // var br1 = readInt()
    // var br2 = readInt()
    // var op = readChar()
    // println(kalkulator(br1,br2,op))

    // print(isNumber('a'))

    // print(transformisiPar((23,12),4))

    // var l1 = spljosti(List(1,1,1,2,2,3,3,4,5))
    // print(l1)
    }
    

    //1) 
    def tipKaraktera(karakter: Char) : String = {
        if(karakter >= 'a' && karakter <= 'z')
            "Malo slovo"
        else if(karakter >= 'A' && karakter <= 'Z')
            "Veliko slovo"
        else
            "Nije slovo"
    }

    //2)
    def pomnoziSa(n : Int, brojevi : Array[Int]) : Int = {
        var rez = n;
        for(broj <- brojevi)
            rez *= broj
        rez
    }

    //3)
    def kvadriraj(brojevi : Array[Int]) : Array[Int] = {
        for(broj <- brojevi)
            yield broj * broj //vracamo u novu kolekciju
    }

    //4)
    def stampajNPuta(c : Char, n : Int) = {
        var i = 0;
        while(i < n) {
            print(c)
            i += 1
        }
    }

    //5)
    def kalkulator( a : Int, b : Int, operacija : Char) = {
        operacija match {
            case '+' => a + b;
            case '-' => a - b;
            case '*' => a * b;
            case '/' => a / b;
            case _ => 0;
        }
    }
    //6)
    def isNumber(a : Any) = {
        a match {
            case n : Int => "Int je"
            case n : Float => "Float je"
            case n : Double => "Double je"
            case _ => "Nije broj"
        }
    }

    //7)
    def transformisiPar(par : Tuple2[Int,Int], n: Int) : Tuple2[Int,Int] =
        (par._1 * n, par._2 + n)

    //8) 
    def spljosti(l : List[Int]) : List[Int] = {
        l match {
            case Nil => Nil;
            case (x : Int) :: Nil => x :: Nil
            case (x : Int) :: (y : Int) :: tail =>
                if(x == y)
                    spljosti(y :: tail)
                else
                    x :: spljosti(y :: tail)
        }
    }
}