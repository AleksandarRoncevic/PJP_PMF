

object AppMain {
    def main(args : Array[String]) : Unit = {
        zad1_d()
        zad2_c()
    }

    def zad1_d() = {

        var l = List("Ana","Voli","Milo4va6na")

        def obrniStringove(l : List[String]) : List[String] = {
            l.map(obrniPomoc)
        }

        def obrniPomoc(str : String) : String = {
            var i = 0;
            for(c <- str) {
                if(Character.isDigit(c))
                    i += 1
            }
            if(i >= 2)
                str.reverse
            else str
        }

        def f1(l : List[String]) : String = {
            obrniStringove(l).reduce((x,y) => x + "-" + y)
        }
        print(f1(l))
    }

    abstract class Teren(val sirina : Double, val duzina : Double, val cena : Double);
    case class KosarkaskiTeren(s : Double, d : Double, c : Double) extends Teren(s,d,c);
    case class FudbalskiTeren(s : Double, d : Double, c : Double, val veliki : Boolean) extends Teren(s,d,c);
    case class OdbojkaskiTeren(s : Double, d : Double, c : Double, val visina : Double) extends Teren(s,d,c);

    def zad2_c() : Unit = {

        var listaTerena : List[Teren] = List(
            KosarkaskiTeren(24.0,50.0,459), //ovaj treba da ostane
            FudbalskiTeren(30.0,60.0,1200,true), //ovaj treba da ostane
            FudbalskiTeren(30.0,60.0,2200,true),
            FudbalskiTeren(30.0,60.0,1200,false),
            OdbojkaskiTeren(24.0,45.0,450,2.5),
            OdbojkaskiTeren(24.0,45.0,550,2.5) //ovaj treba da ostane
        )
        println(f2(listaTerena))
    }
    def f2(l : List[Teren]) : Double = {
        var l2 = l.filter(terenFilter)
        l2.map(_.cena).reduce(_+_) / l2.length
    }

    def terenFilter(t : Teren) : Boolean = {
        t match {
            case KosarkaskiTeren(_,_,_) => true
            case FudbalskiTeren(_,_,c,velik) => c <= 2000 && velik
            case OdbojkaskiTeren(_,_,c,_) => c > 500
            case _ => false
        }
    }

}