# SachoveKoncovky
zápočtový program z Neprocedurálneho programovania, letný semester 2019/2020

šachové koncovky (D,K vs. K) a (V,V,K vs. K)
<br>


Program pomocou funkcie **dajMat** po zadaní šachovej pozície vypíše dohratie partie až do matu. Prvým argumentom je zoznam bielych figúr a druhým argumentom je pozícia kráľa na šachovnici. Program rozoznáva koncovky dámy a kráľa proti kráľovi a koncovky dvoch veží a kráľa proti kráľovi.

### Príklad
-- >>> dajMat ["Va3", "Vg2", "Kc4"] "Kh1"

-- Vb2     Kg1

-- Va1#

## Prehľad dôležitých funkcií a dátových štruktúr
### Výpočet pozície figúrky a jej možností pohybu
Pre výpočet samotných ťahov budeme mať informácie o figúrke uložené v tvare **(Pozicia, [Policko])**, kde **Pozicia** obsahuje typ figúrky (K/D/V) a políčko, na ktorom stojí; 
a **[Policko]** obsahuje všetky políčka, na ktoré sa daná figúrka zo svojej pozície môže pohnúť.

 - **polickaPreFigurku :: Pozicia -> (Pozicia, [Policko])** - funkcia na vstupe dostane pozíciu danej figúrky a vypočíta pre ňu potenciálne políčka, na ktoré sa môže pohnúť. Podľa typu figúrky sa zavolá pomocné funkcie pre jednotlivé figúrky (**polickaPreKrala**, **polickaPreDamu**, **polickaPreVezu**).
 Táto funkcia ešte neberie ohľad na rozloženie ostatných figúr, ktoré sa navzájom môžu svojimi pozíciami blokovať. Skutočné možnosti pohybu figúr zaobstará nasledujúca funkcia.
 
 - **napadnutePoliaFigur :: ([(Pozicia, [Policko])], (Pozicia, [Policko])) -> ([(Pozicia, [Policko])], (Pozicia, [Policko]))** - funkcia dostane na vstup tuple **(biele figúrky, čierna figúrka)** a vráti tuple tých istých figúr so skutočnými možnosťami pohybu v ich zozname **[Policko]**. Odstráni políčka, na ktoré sa figúrka nedostane kvôli inej blokujúcej figúrke a taktiež kráľom zakáže vstup na ohrozené polia.
 
 - **jeSach :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> Bool** - funkcia zistí, či je v zadanej pozícii čierny kráľ v šachu.
 
 - **jeMat :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> Bool** - funkcia zistí, či je zadaná pozícia matová.
 
 - **zoradPolicka :: [(Pozicia, [Policko])] -> (Pozicia, [Policko]) -> (Pozicia, [Policko])** - funkcia pre prvú figúrku v zozname bieleho zoradí jej zoznam [Policko] podľa toho, aký má políčko potenciál byť definitívne vybraté ako nasledujúci ťah. 
    Pre dámu sa podľa jej vzájomnej pozície s čiernym kráľom určí roh, do ktorého kráľa chceme zatlačiť. Stratégiou je približovať sa čo najviac k čiernemu kráľovi smerom k zatlačovaciemu rohu bez šachu, až kým sa nedostaneme na "checkpoint", čo je najbližšia nepatová pozícia dámy k rohu. Napríklad pre čierneho na Kh1 sú checkpointy políčka dámy De2 a Dg4.
    Pre kráľa sa políčka zoraďujú tak, aby neprekážali dáme pri zatlačovaní čierneho kráľa do rohu, a aby bolo políčko čo najbližšie k zatlačovaciemu kraju.
 
 ### Výber nasledujúceho ťahu
  - **zbierajTahy :: ([(Pozicia, [Policko])], (Pozicia, [Policko])) -> [Tah]** - funkcia vypočíta ťahy vedúce k matu. Podľa toho, s akými figúrkami ideme matovať, zavolá buď funkciu **matDaKZac** alebo **matVaV**, a pre čierneho kráľa **utekKrala**. V prípade matovania vežami funkcia podľa pozície kráľov určí, či budú zatláčať kráľa po riadkoch alebo po stĺpcoch. Podobne aj v nasledujúcej funkcií je cieľom prvého ťahu dostať matovacie figúrky medzi kráľov tak, aby nám vlastný kráľ nezavadzal, kým ho nebudeme potrebovať. Výstupom je odohraný ťah bieleho a čierneho.
 
  - **matDaKZac :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> (Pozicia,([(Pozicia, [Policko])],(Pozicia, [Policko])))** - úvodná funkcia na matovanie dámou a kráľom. na začiatku funkcia vyberie pre dámu políčko, ktoré sa nachádza medzi kráľmi tak, aby biely kráľ dáme nezavadzal pri zatláčaní. Ak už dáma medzi kráľmi je, funkcia sa prepne na nasledujúcu funkciu, ktorá bude používaná po zvyšok ťahov. Funkcia vráti zahratý ťah a novú pozíciu figúr na šachovnici.
  - **matDaK :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> (Pozicia,([(Pozicia, [Policko])],(Pozicia, [Policko])))** - funkcia vyberie ďalší ťah bieleho nasledovne:
     - ak je prvá figúrka v zozname dáma, prioritne hľadáme vhodné políčko pre pohyb dámou. Ak neexistujú vhodnejšie políčka ako to, na ktorom dáma už je, funkcia skúsi najsť najlepší ťah pre bieleho kráľa. Nevhodné políčka sú vzdialenejšie od čierneho kráľa; také políčka, že dáme kráľovi šach, ale nedáme mat; alebo políčka, ktoré sú napadnuté súperom a nekryté našou figúrkou.
     - ak je prvá figúrka v zozname kráľ, hľadáme prioritne vhodné políčko pre kráľa. Ak sme sa dostali kráľom tak blízko, že môžeme dámou matovať, funkcia vyberie ťah pre dámu.
   
  - **matVaV :: ([(Pozicia,[Policko])],(Pozicia, [Policko])) -> Smer -> (Pozicia,([(Pozicia,[Policko])],(Pozicia, [Policko])))** - funkcia pre matovanie dvomi vežami. Podľa zadaného smeru bude zatláčať buď po riadkoch alebo po stĺpcoch následovne (predpokladáme zatlačovanie po riadkoch):
     - ak pod kráľom nie je veža, pokúsime sa dostať nejakú veži na riadok pod kráľom
     - ak je už nejaká veža riadok pod kráľom, snažíme sa druhou vežou dať kráľovi šach a zatlačiť ho o riadok vyššie.
     - ak je nejaká z veží v ohrození, presunie sa na druhú stranu riadku tak, aby nezavadzala druhej veži (veža zavadzia druhej veži, ak sú na jednom stĺpci)
   
  - **utekKrala :: ([(Pozicia, [Policko])], (Pozicia,[Policko])) -> ([(Pozicia, [Policko])],(Pozicia,[Policko]))** - funkcia vyberie pre čierneho kráľa nasledujúci ťah. Vyberá také políčko, ktoré nie je ohrozené a ktoré je čo najbližšie stredu šachovnice (aby nám čo najviac pri matovaní komplikoval život).

### Spracovanie výsledku
 - **vypisVysledok :: [Tah] -> IO()** - Výsledkom výpočtovej časti je zoznam ťahov **Tah**, ktoré obsahujú ťah bieleho a čierneho. Funkcia vypíše postupnosť ťahov na štandardný výstup.
 
<br>
<br>
Ďalšie príklady výstupov pre rôzne pozície sú na konci skriptu **mat.hs**.
 
