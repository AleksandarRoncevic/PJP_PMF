
object AppMain {
    def main(args : Array[String]) : Unit = {
        // zad1()
        // zad2()
        // zad3()
        // zad4
        zad6()
    }

    def zad1() = {

        var centar : Tuple2[Double,Double] = (2.0,3.0);
        var tacke : List[Tuple2[Double,Double]] = List(
            (4.0,5.0), (2.0,2.0),
            (9.0,4.0), (1.0,13.0),
            (3.0,2.0), (5.0,5.0)
        );
        var dist : Double = 4.0;
        print(izdvojTacke(tacke,centar,dist));
    }

    def izdvojTacke(l : List[Tuple2[Double,Double]], centar : Tuple2[Double,Double], dist : Double) : List[Tuple2[Double,Double]] = {
        // l.filter(manjaUdaljenost(centar,dist)); //preko filtera
        for(point <- l if manjaUdaljenost(centar,dist)(point)) yield point //nacin preko for-each petle

    }
    def manjaUdaljenost(t1 : Tuple2[Double,Double], dist : Double) (t2 : Tuple2[Double,Double]) : Boolean = {
        var dx = t1._1 - t2._1;
        var dy = t1._2 - t2._2;

        dist > Math.sqrt(dx * dx + dy * dy);
    }

    def zad2() = {
    
        var nizStringova = List(
            "Ana Voli Milovana",
            "Riba Ribi Grize rep",
            "na vrh brda vrba mrda"
        );
        println(nizStringova)
        println(rastaviPaSastavi(nizStringova))
    }

    def rastaviPaSastavi(l : List[String]) : String = {
        var rez = l.flatMap(str => str.split(" ")).reduce((x,y) => x + "," + y)
        return rez
    }

    def zad3() = {
        var nizBr : Array[Int] = Array(23,54,27,22,56)

        print(svastaSaBrojevima(nizBr).toList)
        
    }
    def svastaSaBrojevima(l : Array[Int]) : Array[Int] = {
        l.map(promeniBroj).map(okreni).filter( x => x % 2 == 0)
    }

    def promeniBroj(x : Int) : Int = {
        x % 2 match {
            case 0 => x + 1;
            case _ => x * x;
        }
    }

    def okreni(x : Int) : Int = {
        return Integer.parseInt(("" + x).reverse)
    }

    def zad4() = {
        var l = List(1,3,5,6,9)
        var n = 3;
        print(f4(l,n))
    }

    def f4(brojevi : List[Int], n : Int) : List[Tuple2[Int,Int]] = {

        var brojOstataka = new Array[Int](n) //inicijalizacija niza duzine n

        for(num <- brojevi) {
            var ost = num % n;
            brojOstataka(ost) += 1
        } 

        for(index <- (0 until n).toList)
            yield (index, brojOstataka(index))
    }


    //zad 5
    abstract class Naselje(val brStanovnika : Int, val povrsina : Double);

    case class Selo(brS : Int, p : Double, val tip : String) extends Naselje(brS,p)
    case class Varosica(brS : Int, p : Double) extends Naselje(brS,p)
    case class Grad(brS : Int, p : Double, val imaBazen : Boolean) extends Naselje(brS,p)


    def zad6() = {

        var listaNaselja : List[Naselje] = List(
            Selo(150,25,"zbijeno"),
            Selo(200,35,"razbijeno"),
            Varosica(9000,50),
            Grad(200000,150,true),
            Grad(200000,150,false),
        )

        print(f6(listaNaselja))
    }

    def f6(l : List[Naselje]) : List[Naselje] = {
        l match {
            case Nil => Nil;
            case x :: y => {
                x match {
                    case Grad(brS, _, imaBazen) => {
                        if(imaBazen && brS > 150000)
                            return x :: f6(y)
                        else return f6(y)
                    };
                    case Selo(_,_,tip) => {
                        if(tip == "razbijeno") {
                            x :: f6(y)
                        }
                        else f6(y)
                    };
                    case _ => f6(y)
                };
            }
        }
    }

}