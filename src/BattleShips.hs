-- IB015 2019 - Kostra řešení dvanácté domácí úlohy
--   * V kostře nahraďte ‚undefined‘ vlastní implementací.
--   * Definicím funkcí můžete přidávat formální parametry.
--   * DULEŽITÉ: Zadané datové typy nemodifikujte.
--   * DŮLEŽITÉ: Zadaným funkcím neměňte typové signatury.
--   * DŮLEŽITÉ: Ke všem globálně definovaným funkcím uvádějte typovou signaturu.
--   * Řešení si zkuste spustit na Aise s GHC 8.6.
--   * Vyřešenou úlohu nahrajte do odevzdávárny své seminární skupiny.
-- Před tento řádek nic nepřidávejte
import Text.Read
import Data.List


---------------------------------------------------
--      Z A D A N É   D A T O V É   T Y P Y      --
---------------------------------------------------

-- loď se seznamem příslušných koordinátů s pravdivostní hodnotou
-- podle toho, zda jsou zasaženy (True = zasažen, False = nezasažen)
data Ship = Ship [(Coord, Status)]
          deriving (Eq, Show)

-- moře jsou čtvercová
data ShipsPlan = ShipPlan Int [Ship]
               deriving (Eq, Show)

type Coord = (Int, Int)

data ShipOrientation = Horizontal
                     | Vertical
                     deriving (Eq, Show)

type ShipSize = Int

data Status = AsNew
            | Damaged
            deriving (Eq, Show)

data ShotResult = Ocean
                | Hit
                | Sunk
                deriving (Show, Eq)


---------------------------------------------------
--   F U N K C E   K   I M P L E M E N T A C I   --
---------------------------------------------------

--Používam ich vo viacerých funkciách preto som ich definoval globálne
unpackShip :: Ship -> [(Coord, Status)]
unpackShip (Ship x) = x

packShip :: [(Coord, Status)] -> Ship
packShip x = Ship x


isEmpty :: ShipsPlan -> Bool
isEmpty (ShipPlan _ []) = True
isEmpty _ = False


toShip :: Coord -> ShipOrientation -> ShipSize -> Ship
toShip (x, y) orient size = Ship $ build (x, y) orient size
                where build _ _ 0 = []
                      build (x, y) Horizontal size = ((x, y), AsNew) : build (x + 1, y) Horizontal (size - 1)
                      build (x, y) Vertical size = ((x, y), AsNew) : build (x, y + 1) Vertical (size - 1)



--Sla by definovat aj lokalne ale je trochu zlozitejsia plus vola dalsiu funkciu, preto som ju definoval globalne 
--Funkcia vyfiltruje suradnice novej lode a vsetky suradnice ktore su aktualne na hernom plane obsadene, potom spravi ich prienik ak je [] (lode nekoliduju) tak ju pripoji 
--k hernemu planu, ak je neprazdny, lode by niekde kolidovali preto vrati Nothing.
--Dovod preco som vytvoril tuto funkciu je ze v povodnej najprv kontrolujem ci je vkladana lod vobec validna vzhladom na parametre planu, pretoze toShip kontrolu nerobi

checkCollision :: Coord -> ShipOrientation -> ShipSize -> ShipsPlan -> Maybe ShipsPlan
checkCollision (x, y) orientation size (ShipPlan ocean list) = let newShip = toShip (x, y) orientation size in
                                                               if null ((map fst . unpackShip) newShip `intersect` (map fst . foldl (++) [] . map unpackShip) list) 
                                                                  then Just $ ShipPlan ocean $ newShip : list
                                                                  else Nothing



--Kontroluje len parametre vkladanej lode (pokial je typu Horizontal, zvacsuje sa len x-ova suradnica, y-ova sa nemeni... a naopak pri Vertical) preto je podmienka tak
--zvlastne definovana - pri Horizontal sa y nemeni preto podmienku "y + size - 1 > ocean" nesmie brat do uvahy... a naopak pri Vertical
--Tiez treba skontrolovat zaciatocnu poziciu lode ci nieje mimo plan a tiez ci je size kladna

placeShip :: Coord -> ShipOrientation -> ShipSize -> ShipsPlan -> Maybe ShipsPlan
placeShip (x, y) orientation size (ShipPlan ocean list) = let planSize = [1..ocean] in
          if (x + size - 1 > ocean && orientation == Horizontal) || (y + size - 1 > ocean && orientation == Vertical) || notElem x planSize || notElem y planSize || size < 1
             then Nothing 
             else checkCollision (x, y) orientation size (ShipPlan ocean list) 
          


-- Co robi newList:
--   1. map (searchCoord (x, y) . unpackShip) - kazdu lod vybali zo Ship, nasledne rekurzivne prejde zoznam pozicii a statusov, ak najde zhodu s vystrelom zmeni status na Damaged
--   2. filter (or . map (\(_, status) -> status /= Damaged)) - odstrani potopene lode (kazdu prejde tak, ze porovna statusy s Damaged, "or" zabezpeci ze ked su vsetky statusy Damaged,
--      tak celu lod odstrani z planu, ak vsak coilen jedna je rozna od Damaged, lod v plane ponecha
--   3. map packShip - kazdu lod naspat zabali do Ship

-- Co robi result:
--   Zoberie povodny a upraveny zoznam lodi, pokial su zhodne (ziadny prvok sa nezmenil) mohli nastat dve situacie:
--           1. Trafili sme lod ale dana pozicia uz bola Damaged, to koltroluje "elem (x, y) ((concat . map (map fst . unpackShip)) list)" ak je ciel v zozname suradnic lodi vrati Hit
--           2. Trafili sme ocean (ak ciel nieje v zozname suradnic lodi)
--                                           pokial sa zoznamy lisia ale maju rovnaku dlzku (ziadnu lod sme neodstranili) tak sme trafili AsNew poziciu... vrati Hit
--                                           pokial su zoznamy roznej dlzky, potopili sme nejaku lod... vrati Sunk
          
shoot :: Coord -> ShipsPlan -> (ShipsPlan, ShotResult)
shoot (x, y) (ShipPlan ocean list) = let newList = (map packShip . filter (or . map (\(_, status) -> status /= Damaged)) . map (searchCoord (x, y) . unpackShip)) list 
                                     in (ShipPlan ocean newList, result (x, y) list newList)
                    
      where result (x, y) list newList | list == newList = if elem (x, y) ((concat . map (map fst . unpackShip)) list) then Hit else Ocean
                                       | length list == length newList = Hit
                                       | otherwise = Sunk
            searchCoord _ [] = []
            searchCoord (x2, y2) (((x1, y1), status) : r) = if x1 == x2 && y1 == y2 then (((x1, y1), Damaged) : r) else ((x1, y1), status) : searchCoord (x2, y2) r



-- coorsWithStatus - odbali kazdu lod z Ship a spoji suradnice so statusmi do jedneho zoznamu
-- justCoors - to iste ale bez statusov (len suradnice vsetkych obsadenych pozicii)
-- vytvorim 2D maticu (ocean x ocean) potom kazdu poziciu dam do "status" kde kontrolujem ci sa pozicia nachadza v suradniciach kde su lode
--                    1. Ak nie - na danom mieste je ocean... vrati ~
--                    2. Ak ano - vyhladam v coorsWithStatus prislusnu suradnicu a porovnam s AsNew... podla toho vratim prislusny symbol

printPlan :: ShipsPlan -> IO ()
printPlan (ShipPlan ocean ships) = let coorsWithStatus = (foldl (++) [] . map unpackShip) ships in
                                   (putStr . unlines) [foldl (++) [] [status (x, y) coorsWithStatus | x <- [1..ocean]] | y <- [1..ocean]] 
                                   
                                   where status (x, y) coorsWithStatus = let justCoors = map fst coorsWithStatus in
                                                                         if notElem (x, y) justCoors 
                                                                            then "~"
                                                                            else if (snd . head . filter ((== (x, y)) . fst)) coorsWithStatus == AsNew 
                                                                                    then "#"
                                                                                    else "X"


game :: IO ()
game = undefined


---------------------------------------------------
--         P O M O C N É   F U N K C E           --
---------------------------------------------------

-- Pomocná funkce pro zpracování řádku načteného pro zadání lodě.
-- Funkce řeší pouze zpracování řádku zadávající loď a v případě,
-- že je tento vstup validní, vrací zpracované parametry zabalené
-- v Maybe. V opačném případě vrací Nothing.
-- Možná vás překvapí do-notace bez IO. Ve skutečnosti tu využíváme
-- toho, že Maybe je stejně jako IO tzv. monádou - podrobnosti pře-
-- sahují rámec tohoto kurzu. Nám stačí vědět, že (stejně jako u IO)
-- pokud nějaký z výpočtů selže (takže funkce z níž si vytahujeme
-- hodnotu pomocí "<-" vrátí Nothing), tak selže celá funkce jako
-- celek -> návratová hodnota bude Nothing. Můžete si zkusit volání
-- vyhodnotit:   parseShipInput "3 4 A 10"
-- (Výsledkem bude Nothing - selže parseOrientation. Všimněte si, že
-- není potřeba po každém volání kontrolovat, zdali volání funkce
-- uspělo - o to se nám postará do-notace, resp. funkce (>>) a (>>=)).
parseShipInput :: String -> Maybe (Coord, ShipOrientation, ShipSize)
parseShipInput input = if length inputs /= 4 then Nothing
                       else do
                            x <- readMaybe str_x
                            y <- readMaybe str_y
                            orientation <- parseOrientation str_or
                            size <- readMaybe str_size
                            return ((x, y), orientation, size)
    where
          inputs = words input
          [str_x, str_y, str_or, str_size] = inputs
          parseOrientation "V" = Just Vertical
          parseOrientation "H" = Just Horizontal
          parseOrientation  _  = Nothing


-- Analogicky pomocná funkce pro zpracování řádku načteného pro zadání souřadnic
-- pro střelbu.
parseShootInput :: String -> Maybe Coord
parseShootInput input = if length inputs /= 2 then Nothing
                        else do
                             x <- readMaybe str_x
                             y <- readMaybe str_y
                             return (x, y)
    where inputs = words input
          [str_x, str_y] = inputs


-- při kompilaci pomocí `ghc zadani12.hs -o <jméno_výstupního_souboru>` a následném
-- spuštění výsledné binárky se nám automaticky zavolá funkce game
main :: IO ()
main = game
