
object AppMain {
    def main(args : Array[String]) : Unit = {
        // zad1()
        // zad2()
        // zad3()
        // zad4()
        zad5()

    }

    def zad1_a() = {
        var lista = List((2,4),(5,6),(96,7),(23,1),(4,5))

        def obrniParove(l : List[Tuple2[Int,Int]]) : List[Tuple2[Int,Int]] = {
            l.map(x => obrniPomoc(x))
        }
        
        def obrniPomoc(x : Tuple2[Int,Int]) : Tuple2[Int,Int] = {
            return (x._2*10,x._1)
        }

        // print(obrniParove(lista))

        def f1(l : List[Tuple2[Int,Int]]) : List[Tuple2[Int,Int]] = {
            obrniParove(l).filter(x => x._1 >= x._2)
        }

        print(f1(lista))
    }

    def zad1_b() = {

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

        def f2(l : List[String]) : String = {
            obrniStringove(l).reduce((x,y) => x + "-" + y)
        }
        print(f2(l))
    }

    def zad1_c() = {
        var l = List(List(45,-34,67),List(23,12,-54))

        def obradiListu(l : List[Int]) : List[Int] = {
            l.map(pomocInt)
        }
        def pomocInt(el : Int) : Int = {
            if(el > 0)
                el / 2
            else Math.abs(el)
        }

        def f3(l : List[List[Int]]) = {
            l.map(x => obradiListu(x).reduce(_+_))
        }

        print(f3(l))
    }

    def zad1_d() = { 

        var l : List[List[Double]] = List(List(4.0,3.0,6.7,2.4,3.4),List(3.4,5.6,1.1,1.2,2.2))
        def prosecnaVrednost(l : List[Double]) : Double = {
            l.reduce(_+_) / l.length
        }

        def f4(l : List[List[Double]]) : List[Double] = {
            l.map(prosecnaVrednost).filter(x => x > 0 && x < 3)
        }

        print(f4(l))
    }

    abstract class Artikal(val sifra : String, val tezina : Double, val cena : Double);
    case class Jaje(s : String, t: Double, c : Double, val brojKomada : Int) extends Artikal(s,t,c)
    case class Hleb(s : String, t: Double, c : Double, val beskvasni : Boolean) extends Artikal(s,t,c)
    case class Mleko(s : String, t: Double, c : Double) extends Artikal(s,t,c)

    def zad2_a() = {

        var la : List[Artikal] = List(
            Jaje("2",1.2,210,5),
            Jaje("2",1.2,140,5),
            Jaje("2",1.2,35,12),
            Mleko("1",2.0,120),
            Hleb("3",0.6,120,true),
            Hleb("3",0.6,120,false)
        )

        def obradiArtikle(l : List[Artikal]) : Double = {
            var nl = l.filter(filterArtikal)
            var sum : Double = 0.0
            for(el <- nl) sum += el.cena
            sum

        }

        def filterArtikal(a : Artikal) : Boolean = {
            a match {
                case Jaje(_,_,c,brKom) => {
                    (c < 150 && brKom > 10) || c > 200
                }
                case Hleb(_,t,_,kvas) => {
                    t > 0.5 && kvas
                }
                case _ => false
            }
        }
        println(obradiArtikle(la))
    }


    def tokenizacija(s: String) : Array[String] = s.toLowerCase.split(" ")

	def glavna(p: List[Tuple2[Int, String]]) = p
		.flatMap{
			case (id, tekst) => tokenizacija(tekst).map(w => (w, id))
		}
		.groupBy(_._1)
		.map{
			case (w, p) => (w, p.map(_._2).toList)
		}
		.toList

	def zad3_a() = {
		var p = List(
			(1, "Ana voli milovana"),
			(2, "Milovana voli ana"),
			(3, "Milovana boli ruka")
		)

		println(glavna(p))
	}

}