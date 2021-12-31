
object AppMain {
    def main( args : Array[String]) : Unit = {
        var l1 = List(1,2,3)
        var l2 = List(4,8,16)
        var l4 = List(4,5,6,5)
        var l3 = List(l1,l2,l4)

        var string = ("aNa vOlI mILovAna".toArray).toList

        var nizStr =(List("ana","voli","milovan"))
        print(svastaSaBrojevima(l4))
        println
        println
        print(filtrirajChar(string))

        println
        println
        println(prosecnaDuzina(nizStr))

        var lDugi = Array(12345,11131,353531,4455664)
        println
        println
        println(svastaSaBrojevima2(lDugi).toList)

        var nizStringova = List(
            "Ana Voli Milovana",
            "Riba Ribi Grize REP",
            "na vrh brda vrba mrda"
        );
        println
        println
        println(svastaSaStringovima(nizStringova))

    }

    //1)
    def svastaSaBrojevima(l : List[Int]) : List[Int] = {
        if(l.length % 2 == 0) {
            l.map(x => x * x)
        } else {
            l.map(x => x * 10)
        }
    }

    //2)
    def filtrirajChar(l : List[Char]) : List[Char] = {
        l.filter((c : Char) => c.isLower).map((c : Char) => c.toUpper)
    }

    //3)
    def prosecnaDuzina(l : List[String]) : Double = {
        l.map((str : String) => str.length).reduce(_+_) / l.length.toFloat
    }

    //4)
    def svastaSaBrojevima2(l : Array[Int]) : Array[Int] = {
        l.filter(isteCifre).map(okreniBroj)
    }

    def isteCifre(n : Int) : Boolean = {
        if(n < 10) true
        else {
            var poslCifra = n % 10
            var kopija = n
            while( kopija > 9 ) {
                kopija = kopija / 10
            }
            kopija == poslCifra
        }
    }
    
    def okreniBroj(n : Int) : Int = {

        var okrenut = 0;
        var kopija = n;
        while(kopija > 0) {
            var cifra = kopija % 10
            okrenut = okrenut * 10 + cifra
            kopija = kopija / 10
        }
        okrenut
    }

    //5
    def svastaSaStringovima(l : List[String]) : String = {
        
        def prebrojVelike(s : String) : Int = {
            s.toList.filter((c : Char) => c.isUpper).length
        }

        l.filter(str => prebrojVelike(str) < 5)
            .map((str : String) => str + str.reverse)
            .reduce(_+_)
    }
}