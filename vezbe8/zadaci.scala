
object AppMain {
    def main(args : Array[String]) : Unit = {
        // zad2()
        zad6()
    }

    //1)

    abstract class Skola( val ime: String, val brUcenika: Int );

    case class OsnovnaSkola(i: String, brU: Int) extends Skola(i, brU);
    case class SrednjaSkola(i: String, brU: Int) extends Skola(i, brU);

    //2)

    def zad2() = {

        var lista : List[Skola] = List(
            new OsnovnaSkola("Djura",400),
            new OsnovnaSkola("Petefi",600),
            new SrednjaSkola("JJZ",900),
            new OsnovnaSkola("Crnjanski",250)
        );
        ispisiSkole(lista);
        print(izbaciMaleSkole(lista))
    }

    def ispisiSkole(l : List[Skola]) : Unit = {
        l match {
            case Nil => return
            case h :: t => {
                //h je skola, treba utvrditi da li je srednja ili osnovna
                h match {
                    case OsnovnaSkola(ime, brUcenika) => {
                        println("Osnovna skola "+ ime + " ima " + brUcenika + " ucenika.")
                    }

                    case SrednjaSkola(ime,brUcenika) => {
                        println("Srednja skola "+ ime + " ima " + brUcenika + " ucenika.")
                    }
                }
                ispisiSkole(t)
            }
        }
    }

    //3)
    def izbaciMaleSkole(l : List[Skola]) : List[Skola] = {
        l.filter(skola => skola.brUcenika >= 300)
    }


    //4)
    abstract class NebeskoTelo(val ime : String, val precnik : Int);

    case class Planeta(i: String, p: Int, val imaVodu : Boolean) extends NebeskoTelo(i,p);
    case class Satelit(i: String, p: Int, val kruziOko: String) extends NebeskoTelo(i,p);

    //5)
    def zad5() : Unit = {
        var listaNebeskih : List[NebeskoTelo] = List(
            Planeta("Zemlja",6500,true),
            Satelit("Mesec",1700,"Zemlja"),
            Planeta("Jupiter",70000,false),
            Satelit("Evropa",1600,"Jupiter"),
            Satelit("Io",1800,"Jupiter")
        );

        println
        println
        print(vratiSatelitePlanete(listaNebeskih, "Zemlja"))
    }

    def vratiSatelitePlanete(l : List[NebeskoTelo], imePl : String) : List[NebeskoTelo] = {
        l.filter(filterPomoc(imePl))
    }

    def filterPomoc (imePl : String) (nt : NebeskoTelo) : Boolean = {
        nt match {
            case Satelit(ime,precnik,planeta) => {
                if(planeta == imePl) 
                    true
                else 
                    false
            }
            case _ => false;
        }
    }

    //6)
    def zad6() : Unit = {
        var listaNebeskih : List[NebeskoTelo] = List(
            Planeta("Zemlja",6500,true),
            Satelit("Mesec",1700,"Zemlja"),
            Planeta("Jupiter",70000,false),
            Satelit("Evropa",1600,"Jupiter"),
            Satelit("Io",800,"Jupiter")
        );
        println
        println
        println(filtrirajNT2(listaNebeskih))
    }

    def filtrirajNT2(l : List[NebeskoTelo]) : List[NebeskoTelo] = {
        l.filter(filterPomoc2)
    }

    def filterPomoc2(nt : NebeskoTelo) : Boolean = {
        nt match {
            case Planeta(ime,precnik,imaVodu) => {
                precnik > 5500 && imaVodu
            }
            case Satelit(ime,precnik,imePl) => {
                precnik < 1000
            }
        }
    }
}
