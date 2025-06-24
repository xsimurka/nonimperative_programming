-- IB015 2019 - Kostra řešení čtvrté domácí úlohy
--   * V kostře nahraďte ‚undefined‘ vlastní implementací.
--   * Definicím funkcí můžete přidávat formální parametry.
--   * DŮLEŽITÉ: Neodstraňujte žádné zadané funkce.
--   * DŮLEŽITÉ: Ke všem funkcím uvádějte typovou signaturu.
--   * Řešení si zkuste spustit na Aise s GHC 8.6.
--   * Vyřešenou úlohu nahrajte do odevzdávárny své seminární skupiny.

-- Před tento řádek nic nepřidávejte
import Data.List

type ShowID = Int
type ShowName = String
type Rating = Int
type EpisodeNumber = Int
type Year = Int
type EpisodeName = String

type ShowInfo = (ShowID, ShowName, Rating)
type EpisodeInfo = (ShowID, EpisodeNumber, Year, EpisodeName)

type Shows = [ShowInfo]
type Episodes = [EpisodeInfo]


findShowId :: Shows -> ShowName -> ShowID
findShowId ((a, b, c) : r) name = if name == b then a else findShowId r name
findShowId [] name = -1

getEpisodes :: Episodes -> ShowID -> Episodes
getEpisodes ((a, b, c, d) : r) num = if num == a then (a, b, c, d) : getEpisodes r num else getEpisodes r num
getEpisodes [] num = []  

countEpisodes :: Episodes -> ShowID -> Int
countEpisodes ((a, b, c, d) : r) num = length (getEpisodes ((a, b, c, d) : r) num)
countEpisodes [] num = 0

orderByEpsNumber :: Episodes -> [(EpisodeNumber, ShowID, Year, EpisodeName)]
orderByEpsNumber ((a, b, c, d) : r) = (b, a, c, d) : orderByEpsNumber r
orderByEpsNumber [] = [] 

getEpsNumber :: (EpisodeNumber, ShowID, Year, EpisodeName) -> EpisodeNumber
getEpsNumber (a, b, c, d) = a

isShowContiguous :: Episodes -> ShowID -> Bool
isShowContiguous ((a, b, c, d) : r) num = if length (getEpisodes ((a, b, c, d) : r) num) == 0 then True else if (-) (getEpsNumber (maximum (orderByEpsNumber (getEpisodes ((a, b, c, d) : r) num)))) (getEpsNumber (minimum (orderByEpsNumber (getEpisodes ((a, b, c, d) : r) num)))) == (-) (length (getEpisodes ((a, b, c, d) : r) num)) 1 then True else False 
isShowContiguous [] num = True 

getYear :: Episodes -> [Int]
getYear ((a, b, c, d) : r) = c : getYear r
getYear [] = []

publicationRange :: Episodes -> ShowID -> (Year, Year)
publicationRange ((a, b, c, d) : r) num = (minimum ((getYear (getEpisodes ((a, b, c, d) : r) num))), maximum ((getYear (getEpisodes ((a, b, c, d) : r) num))))

getShowName :: (Rating, ShowName) -> ShowName
getShowName (a, b) = b

changeOrder :: Shows -> [(Rating, ShowName)]
changeOrder ((a, b, c) : r) = (c, b) : changeOrder r
changeOrder [] = []

bestRating :: Shows -> ShowName
bestRating ((a, b, c) : r) = getShowName (maximum (changeOrder ((a, b, c) : r)))

worstRating :: Shows -> ShowName
worstRating ((a, b, c) : r) = getShowName (minimum (changeOrder ((a, b, c) : r)))

change :: Episodes -> [(Year, ShowID, EpisodeNumber, EpisodeName)]
change ((a, b, c, d) : r) = (c, a, b, d) : change r
change [] = []

rechange :: [(Year, ShowID, EpisodeNumber, EpisodeName)] -> Episodes
rechange ((c, a, b, d) : r) = (a, b, c, d) : rechange r
rechange [] = []

sortByYearOfPublication :: Episodes -> Episodes
sortByYearOfPublication ((a,b,c,d):r) = rechange (sortOn f (change ((a,b,c,d):r))) where f (a,b,c,d) = a
sortByYearOfPublication [] = []

getNamesOfEpisodes :: Episodes -> [EpisodeName]
getNamesOfEpisodes ((a,b,c,d):r) = d : getNamesOfEpisodes r
getNamesOfEpisodes [] = []

oneShowEpisodes :: ShowInfo -> Episodes -> [(ShowName, [EpisodeName])]
oneShowEpisodes (x,y,z) ((a,b,c,d):r) = [(y, getNamesOfEpisodes (getEpisodes ((a,b,c,d):r) x))]
oneShowEpisodes (x,y,z) _ = []

showEpisodes :: Shows -> Episodes -> [(ShowName, [EpisodeName])]
showEpisodes ((x,y,z):t) ((a,b,c,d):r) = oneShowEpisodes (x,y,z) ((a,b,c,d):r) ++ showEpisodes t ((a,b,c,d):r)
showEpisodes [] ((a,b,c,d):r) = []
showEpisodes ((x,y,z):t) [] = [(y, [])] ++ showEpisodes t []
showEpisodes [] [] = []

oneShowJoin :: ShowInfo -> Episodes -> [(ShowName, EpisodeName)]
oneShowJoin (x,y,z) ((a,b,c,d):r) = if x == a then (y, d) : oneShowJoin (x,y,z) r else oneShowJoin (x,y,z) r
oneShowJoin (x,y,z) _ = []

join :: Shows -> Episodes -> [(ShowName, EpisodeName)]
join ((x,y,z):t) ((a,b,c,d):r) = oneShowJoin (x,y,z) ((a,b,c,d):r) ++ join t ((a,b,c,d):r) 
join _ _ = []



