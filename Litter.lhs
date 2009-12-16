This is a simple standalone program to help write literate programs in block based programming languages.
It is mainly useful for those programming languages where anything between \type{/*} and \type{*/}  AND anything between \type{//} and a {\em new line}
are comments.

Currently, this program is hardcoded w.r.t the typesetting mechanism --- you can only use it with \CONTEXT\ . Which is not a big loss if you are a \TeX\ fan.
\begin{code}
module Main where
\end{code}

\section{imports}\index{imports}
We need to import the usual parser modules, followed by {\em Monad} , {\em Data.Map} and some usual suspects w.r.t file handling . Finally, we need 
the {\em ByteString} to have a fast string handling.
\begin{code}
import Text.ParserCombinators.Parsec as C
import Data.Char
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(haskellDef)
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import System.Environment(getArgs,getProgName,getEnv)
import qualified System.Directory as D
import qualified System.IO as I
import qualified System.FilePath as FPath
import qualified System.Directory as DIR
\end{code}

\section[inpFormat]{Input format}\index{input,format}

from the point of view of the \LiTTeR\ point of view, a valid literate program would consist of an optional prelude, followed by a  list of {\sl sections}.

\subsection[prelude]{Prelude}\index{prelude}
The prelude is the place where you can define the title page, additional \TEX\ or \CONTEXT\ commands. This will be ignored will generating the program,
but will be placed in (almost) the beginning of the generated \TEX\ document. {\bf ADD LINK HERE}
Prelude is typed in as 
\starttyping
/*P<any number of any characters or new lines or whateva>P*/
\stoptyyping

\subsection[section]{Sections}\index{section}
With \LiTTeR\ you develop your program in {\em  sections}. Each section consists of an exposition in \TEX\ or \CONTEXT , followed by code in the formal programming language,
which is then followed by , what is defined as , {\sc meta} block.

Briefly, The structure of each section is given below:
\starttyping
/*LTEX(t): <any characters, new lines etc (but not 'L*/' >
L*/CODE:<any number of any characters other than '/*M'>
/*M META: a defined set of commands M*/
\stoptyping

From the above section, \LiTTeR\ reads the \quotation{META} data and accordingly,
takes the \quotation{CODE} and places it in the program file. The \quotation{TEX(t)} is copied as it is and is placed in the \CONTEXT\ file , again as per the directives
in the \quotation{META}.

\subsection[link]{Links}\index{link}
Occassionally, there are situations where the code should be placed in a particular order but is better documented in a different order.
For instance, before we explain input validations, it might be better to explain the logic of a function first, then explain what sort of input validation is needed and
why. In such cases, it would be nice the document and the code differ from each other --- the document contains just a {\em link} to a piece of code but the program file 
contains it as it is.  This is achieved in \LiTTeR\ via , what is defined , as links. The way to define link is 
\starttyping
//L link(<identifier>);
\stoptyping

\subsection[file]{file}\index{program files}
In many occassions, (too many, in case of java), a bunch of closely related pieces of code occupy different files in the file system.
\startitemize[2,packed]
\item In java, each public class occupies its own file --- it is perhaps useful to explain all or some of them together.
\item It is perhaps useful to explain a class file and its unit test in the same document
\item In case of C and C++ , it is useful to explain the header file and the c file in the same document.
\stopitemize

In \LiTTeR\ we use the following directive to indicate the name of the program file.
\starttyping
//L file(FILENAME:<a sequence of any number of any chars>)
\stoptyping
This would ensure that all the code {\sl sections} that follow are placed in the file \quotation{FILENAME}. At least one \type{//L file} is needed and it should be placed
right after the {\em prelude} (spaces,newlines etc.~are allowed, ofcourse)

\subsection[fullinput]{Full Input}\index{complete structure}
The complete structure is defined as below
/starttyping
<XET:> PRELUDE? [CODESECTION]+
<PRELUDE:>   /*P ..... P*/
<CODESECTION:> //L file(..) SECTIONS
<SECTIONS:> /*L ... L*/..../*M..M*/
/stoptyping

\begin{code}
data LFragment = Literate{ laTex :: B.ByteString,
                           code :: B.ByteString,
                           metaData :: ProgElem}
                  deriving Show

data ProgInfo = Link String
                | ProgEnd String
                | Elem LFragment
                deriving Show

data ProgElem = ProgElement { elementType :: ElementType,
                              elemName    :: String,
                              refers      :: [String],
                              elemInclude  :: Bool 
                            }

showPElem (ProgElement et en _ _) = en ++ " " ++ (show et)

instance Show ProgElem where
 show = showPElem


data ElementType = Klass 
                   | Trait 
                   | Struct 
                   | Method 
                   | GlobMethod 
                   | StaticVar 
                   | StaticConst 
                   | GlobalConst 
                   | InstVar 
                   | InstVal
                   | Header
                   | Block

instance Show ElementType where
  show = showElemType

showElemType Klass = "class"
showElemType Trait = "trait"
showElemType Struct = "structure"
showElemType Method = "method"
showElemType GlobMethod = "global method"
showElemType StaticVar = "static variable"
showElemType StaticConst = "static constant"
showElemType InstVar  = "instance variable"
showElemType InstVal  = "instance value"
showElemType Header = "Header"
showElemType Block = ""

data CodeSection = CSection String [ProgInfo]
                   deriving Show
\end{code}

\begin{code}
literateComments = do string "/*L"
                      comms <- manyTill anyChar (try (string "L*/"))
                      return comms

parsePrelude = do string "/*P"
                  comms <- manyTill anyChar (try (string "P*/"))
                  return comms

\end{code}
A parseSection is simply a literateComments followed by any String till /*M, followed by any string till M*/


\begin{code}
parseSection = do comments <- literateComments
                  code <- manyTill anyChar (try (string "/*M"))
                  spaces
                  pe <- parseProgElem
                  spaces
                  string "M*/"
                  spaces
                  return  $ Literate (B.pack comments) (B.pack code) pe
\end{code}


parse Literate is simply a list of many parseSection
\begin{code}
parseLiterate = do sections <- many parseSection
                   many anyChar
                   return sections
\end{code}



\begin{code}
parseProgInfo = try parseLink
               <|> try parseEndBlock
                <|> do ps <- parseSection
                       return $ Elem ps

parseLink = do string "//L"
               spaces
               string "link"
               spaces
               string "("
               spaces 
               ident <- parseIdentifier
               spaces
               string ")"
               spaces
               string ";"
               spaces
               return $ Link ident

parseEndBlock= do string "//I"
                  spaces
                  string "end"
                  spaces
                  string "("
                  spaces 
                  ident <- parseIdentifier
                  spaces
                  string ")"
                  spaces
                  string ";"
                  spaces
                  return $ ProgEnd ident


parseIdentifier = do {
                       c <- validFirstChar;
                       xs <- many (letter <|> digit <|> (char '-') <|> (char '.') <|> (char ':'));
                       spaces;
                       return $ c:xs
                      }

validFirstChar = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
\end{code}



\begin{code}
parseProgElem = do elemType <- parseElementType
                   spaces
                   string "("
                   spaces
                   name <- parseIdentifier
                   spaces
                   string ")"
                   spaces 
                   string ";"
                   spaces
                   refs <- option [] parseRefer
                   spaces 
                   toInclude <- option True parseIsIgnore
                   return  $ ProgElement elemType name refs toInclude 

parseRefer = do    string "refers"
                   spaces 
                   string "("
                   spaces
                   ids <- sepBy parseIdentifier spaces
                   spaces
                   string ")"
                   spaces 
                   string ";"
                   return ids

parseIsIgnore = do string "ignoreMe"
                   string ";"
                   return False
               
\end{code}

\begin{code}
strToElemType "class" = Klass
strToElemType "trait" = Trait
strToElemType "struct" = Struct 
strToElemType "method" = Method
strToElemType "globalMethod" = GlobMethod
strToElemType "staticVar" = StaticVar
strToElemType "staticVal" = StaticConst
strToElemType "globalConst" = GlobalConst
strToElemType "var"  = InstVar
strToElemType "val" = InstVal
strToElemType "header" = Header
strToElemType _        = Block

parseElementType = do str <- parseIdentifier
                      return $ strToElemType str
\end{code}


\begin{code}
parseFile = do string "//L"
               spaces
               string "file"
               spaces
               string "("
               fname <- manyTill anyChar (try (string ")"))
               spaces
               return fname
\end{code}

\begin{code}
parseCodeSection = do fileName <- parseFile
                      lst <- many (try parseProgInfo)
                      return $ CSection fileName lst
                      
\end{code}

A trial parser to testout a few things
\begin{code}
getPrelude ::  String -> IO String
getPrelude input
     = case (parse parsePrelude [] input) of
          Left err -> return ""
          Right x ->  return x 


parseFileContents = do prel <- parsePrelude
                       spaces
                       lst <- many parseCodeSection
                       return (prel,lst)


run ::  String -> IO (String,[CodeSection])
run input
     = case (parse parseFileContents [] input) of
          Left err -> error $ ("parse Error" ++ (show err))
          Right x ->  return x 
\end{code}
A couple of tests


We will have a Map between Identifiers and LFragments

\begin{code}
type Env = M.Map String LFragment

mGet map str = case M.lookup str map of
                Nothing -> error $ ("failed to find the link:" ++ str ++ ": available links are " ++ (show $ M.keys map))
                Just x -> x
\end{code}


\begin{code}
progInfoToCode :: ProgInfo -> Env -> B.ByteString
progInfoToCode (Link str) map = code $ mGet map str
progInfoToCode (ProgEnd s) map = B.concat [ B.pack "\n}//End of ",
                                            B.pack s,
                                            B.pack "\n"
                                          ]
progInfoToCode (Elem (Literate _ c m))  map =
   if (elemInclude m) then c
   else B.empty
\end{code}
\begin{code}
progInfoToTeX :: ProgInfo -> Env -> B.ByteString
progInfoToTeX (ProgEnd str) map = B.pack "" -- Ignore it in the document.
progInfoToTeX (Link str) map = 
  B.concat[ B.pack  "\n\\vskip .5cm\\framed{",
            B.pack "insert code from section~\\in[",
            refName,
            B.pack "], titled \\about[",
            refName,
            B.pack "]"  ,
            B.pack "}\n\n"]
             where refName =  B.pack $ show $ metaData $ mGet map str
progInfoToTeX (Elem (Literate teX c m)) map =
  B.concat [ sectionHeader m, teX, B.pack "\\starttyping\n",c , B.pack "\\stoptyping\n",sectionFooter  m map]


showLink p = B.pack (show p)

sectionHeader :: ProgElem -> B.ByteString
sectionHeader p@(ProgElement et en _ _) =
  B.concat [ B.pack "\\section[" ,
             B.pack refName,
             B.pack "]",
             B.pack "{",
             B.pack en,
             B.pack "}",
             B.pack "\\index{",
             B.pack refName,
             B.pack "}\n" ]
         where refName = en -- show p

sectionFooter p map = 
  if (length . refers) p > 0 then
    B.concat [B.pack "\n",
              B.pack  "\n{\\bf See Also}: \n\\startitemize[r,packed,broad]\n",
              makeRef map (refers p),
              B.pack "\\stopitemize\n\\hrule\n\n"]
  else B.pack "\n\n"

makeRef map []= B.empty
makeRef map (refp:rest) =
   B.concat [ B.pack "\\item section~\\in[",
              refName,
              B.pack "], titled \\about[",
              refName,
              B.pack "]",
              makeRef map rest]
    where refName =  B.pack $ elemName $ metaData $ mGet map refp
\end{code}


\begin{code}
createMap (CSection _ lst) env = createMap' lst env

createMap' [] m = m
createMap' (Link _ : rest) m = createMap' rest m
createMap' (Elem p@(Literate _ _ md):rest) m =
   createMap' rest (M.insert (elemName md) p m)
createMap' (_ : rest) m = createMap' rest m

\end{code}


\begin{code}
csToCode :: CodeSection -> Env -> B.ByteString
csToCode (CSection _ ps) env = 
 B.concat $ map (\x -> progInfoToCode x env) ps

csToTeX :: CodeSection -> Env -> B.ByteString
csToTeX (CSection str ps) env =
 B.concat [ B.pack "\\chapter{",
            B.pack (FPath.dropExtension $ snd $ FPath.splitFileName str),
            B.pack "}",
            B.pack "\\index{",
            B.pack str,
            B.pack "}\n",
            B.pack "\\thinrule\\vskip .5cm\\placecontent\\vskip .5cm\\hrule\\vskip .5cm\n",
            B.concat $ map (\x -> 
                              progInfoToTeX 
                              x 
                              env) 
                            ps,
            B.pack "\n\n%End of chapter",
            B.pack str,
            B.pack "\n"]

\end{code}
overAllHeader
overAllFooter
printing TeX file
printing Code files
\begin{code}
overAllHeader = 
  B.concat [ B.pack "\\starttext\n\\setupcolor[state=start]\\setupinteraction[state=start,",
             B.pack "\n\t\tcolor=green,\n\t\tstyle=bold]\n\\setupfloats[location=middle]" ]

overAllFooter =
 B.pack "\\stopbodymatter\n\\startbackmatter\n\\completeindex\n\\stopbackmatter\n\\stoptext\n\n"


printTeXFile fName prel codeInfos env =
 do  wrHandle <- I.openFile fName I.WriteMode
     printTextList wrHandle $ B.lines fullTex
     I.hClose wrHandle
     return ()
         where fullTex = B.concat[overAllHeader,
                                  B.pack prel,
                                  B.pack "\n\\startbodymatter\n",
                                 (B.concat rest), 
                                 overAllFooter]
               rest  = map (\x -> csToTeX x env) codeInfos

printTextList _  [] = return ()
printTextList w (t:rest) = do B.hPutStrLn w  t
                              printTextList w rest


dirNameAndFileName :: [FilePath] -> FilePath-> (FilePath,FilePath)
dirNameAndFileName [] _  = error "Invalid files list"
dirNameAndFileName [f] sofar = (sofar,FPath.combine sofar f)
dirNameAndFileName (d:rest) sofar = dirNameAndFileName rest (FPath.combine sofar d)


printCodeFiles [] _ = return ()
printCodeFiles (cs@(CSection fName ps):rest) env =
  do let (dirName,newFileName) = dirNameAndFileName (FPath.splitDirectories fName) ""
     DIR.createDirectoryIfMissing True dirName
     B.writeFile newFileName  $ csToCode cs env
     printCodeFiles rest env
\end{code}


readInitTeXt =
  do  progName <- getProgName
      maybePath <- D.findExecutable progName
      case maybePath of
        Nothing  -> error $ progName ++ " not found"
        Just path -> do existsFile <- D.doesFileExist (progName ++ ".init")
                        if (existsFile) 
                         then do p <- readFile (progName ++ ".init")
                                 return (B.pack p)
                         else return B.empty

given, a file, we return the list of CodeSection
\begin{code}
parseInputFile fileName =
 do fileContents <- readFile fileName
    (prel,lst) <- run fileContents
    return (prel,lst, cslistToEnv lst M.empty)

cslistToEnv [] e = e
cslistToEnv (c:rest) e =  cslistToEnv rest (createMap c e )
\end{code}

main method follows:
\begin{code}
main = do args <- getArgs
          case args of
            [fileName] -> handleInput fileName 
            otherwise                -> usage

usage = do progName <- getProgName
           putStrLn "Usage"
           print (progName ++ " <inputFile-Without-Extension>")

handleInput fileName =
 do (prel,csecs,env) <- parseInputFile fileName
    printCodeFiles csecs env
    printTeXFile (FPath.replaceExtension  fileName ".tex" )
                 prel
                 csecs 
                 env 

\end{code}
