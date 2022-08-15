{-# LANGUAGE BlockArguments #-}

import System.IO (openFile, hGetLine, hPutStrLn, hPrint, hClose )
import System.IO.Error ( isDoesNotExistError )
import Control.Exception ( catch )
import GHC.IO.IOMode ( IOMode(ReadMode, WriteMode) )
import System.Process (system)
import Data.List ( intersect, sortBy )
import Data.Function ( on )

type Name = String
type Pontuation = Int
type Turn = Int
type Table = String
data Player = Player Name Pontuation deriving (Show, Read)
type Players = [Player] -- A List of gamers

g1 = [Player "Bilau" 0,Player "Tiago" 0,Player "Xarope" 0,Player "Diego" 0]

getString :: String -> IO String
getString str = do
                putStr str
                getChar -- absorver o \n do putStrLn
                getLine

playerExists :: Players -> Name -> Bool
playerExists [] _ = False
playerExists ((Player n p):xs) name
                             | name == n = True
                             | otherwise = playerExists xs name

playerReg :: Players -> IO Players
playerReg ps = do
                system "cls"
                putStr "\nPlayer name: "
                getChar
                name <- getLine
                if playerExists ps name
                then  do
                       putStrLn "This name already exist, pls choose onother one"
                       system "pause"
                       menu ps
                else  do
                       file <- openFile "game.txt" WriteMode
                       hPrint file (Player name 0:ps)
                       hClose file
                       putStrLn ("User " ++ name ++ " registered successfully")
                       system "pause"
                       menu (Player name 0:ps) -- retorna nova lista para menu

loadGame :: Players -> IO Players
loadGame ps = do
                np1 <- getString "Type the player one name: "
--                putStrLn ("p1 " ++ np1)
                if not (playerExists ps np1) then do
                   putStrLn "This player doesn't exist!"
--                   putStrLn ("p1_if " ++ show np1)
                   system "pause"
                   menu ps
                else do
                   putStr "Type the second player's name: "
                   np2 <- getLine
                   --np2 <- getString "Type the second player's name: "
--                   putStrLn ("p2 " ++ np2)
                   if not (playerExists ps np2) then do
                      putStrLn "The second player doesn't exist!"
                      system "pause"
                      menu ps
                    else
                         newGame ps np1 np2

newGame :: Players -> Name -> Name -> IO Players
newGame ps p1 p2 = do
                     putStrLn ("\nStarting the Game: \"" ++
                      p1 ++ " vs " ++ p2 ++ "\" ... ")
                     putStrLn "\nThe numeric squares are not marked"
                     putStrLn ("\n" ++ p1 ++ " will be the \'X\' and " ++ p2 ++
                                             " will be the \'O\'. Lets go!")
                     runGame ps ['1', '2', '3', '4', '5', '6', '7', '8', '9'] p1 p2 0

playerWin1 :: Table -> Bool
playerWin1 table
        -- verifica primeiro nas linhas
        | (((table !! 0) == 'X') && ((table !! 1) == 'X') && ((table !! 2) == 'X')) = True
        | (((table !! 3) == 'X') && ((table !! 4) == 'X') && ((table !! 5) == 'X')) = True
        | (((table !! 6) == 'X') && ((table !! 7) == 'X') && ((table !! 8) == 'X')) = True
        -- verifica nas colunas
        | (((table !! 0) == 'X') && ((table !! 3) == 'X') && ((table !! 6) == 'X')) = True
        | (((table !! 1) == 'X') && ((table !! 4) == 'X') && ((table !! 7) == 'X')) = True
        | (((table !! 2) == 'X') && ((table !! 5) == 'X') && ((table !! 8) == 'X')) = True
        -- verifica nas diagonais
        | (((table !! 0) == 'X') && ((table !! 4) == 'X') && ((table !! 8) == 'X')) = True
        | (((table !! 2) == 'X') && ((table !! 4) == 'X') && ((table !! 6) == 'X')) = True
        | otherwise = False

playerWin2 :: Table -> Bool
playerWin2 table
        -- verifica primeiro nas linhas, atenção: o índice começa do 0
        | (((table !! 0) == 'O') && ((table !! 1) == 'O') && ((table !! 2) == 'O')) = True
        | (((table !! 3) == 'O') && ((table !! 4) == 'O') && ((table !! 5) == 'O')) = True
        | (((table !! 6) == 'O') && ((table !! 7) == 'O') && ((table !! 8) == 'O')) = True
        -- verifica nas colunas
        | (((table !! 0) == 'O') && ((table !! 3) == 'O') && ((table !! 6) == 'O')) = True
        | (((table !! 1) == 'O') && ((table !! 4) == 'O') && ((table !! 7) == 'O')) = True
        | (((table !! 2) == 'O') && ((table !! 5) == 'O') && ((table !! 8) == 'O')) = True
        -- verifica nas diagonais
        | (((table !! 0) == 'O') && ((table !! 4) == 'O') && ((table !! 8) == 'O')) = True
        | (((table !! 2) == 'O') && ((table !! 4) == 'O') && ((table !! 6) == 'O')) = True
        | otherwise = False

-- função que atualiza pontuação do vencedor
-- recebe a lista (Jogadores), o nome do vencedor e retorna uma nova lista atualizada
updatePontuation :: Players -> String -> Players
updatePontuation ((Player nm pt):xs) winner
                               | (nm == winner) = [(Player nm (pt + 1))] ++ xs
                               | otherwise = (Player nm pt):(updatePontuation xs winner)

-- exibir ranking dos jogadores
-- critério: da maior para a menor pontuação
listRanking :: Players -> IO ()
listRanking [] = return ()
listRanking (x:xs) = do
                     --putStrLn ((getName x) ++ " has " ++ (show (getPontuation x)) ++ " points")
                    putStrLn ( nm ++ "has " ++ (show (getPontuation x)) ++ " points")
                    listRanking xs
                     where
                          --space10 = "          "
                          --lspace = length space10
                          lname  = length (getName x)
                          ll = 10 - lname
                          space = [ " " | sp <- [1..ll]]
                          --foldl (\b a -> b ++ a) "Diego" [" ", " ", ""]
                          nm = foldl (++) (getName x) space

-- função que recebe um jogador e retorna o nome
getName :: Player -> Name
getName (Player nome _) = nome

-- função que recebe um Player e retorna a pontuação
getPontuation :: Player -> Pontuation
getPontuation (Player _ pontuacao) = pontuacao

-- função que define o critério de ordenação
ordenar :: Players -> Players
ordenar pls = sortBy (compare `on` getPontuation) pls

-- essa função recebe uma lista com a configuração do tabuleiro,
-- a vez, um elemento (opção escolhida pelo jogador), retorna uma nova configuração (nova lista)
getNewTab :: Table -> Turn -> Char -> Table
getNewTab (x:xs) turn e
                      | ((x == e) && (turn == 0)) = (['X'] ++ xs)
                      | ((x == e) && (turn == 1)) = (['O'] ++ xs)
                      | otherwise = x:(getNewTab xs turn e)

runGame :: Players -> Table -> Name -> Name -> Turn -> IO Players
runGame ps tb n1 n2 tu = do
    -- imprime o tabuleiro
    putStrLn ("\n" ++ "                              " ++
             (show (tb !! 0)) ++ " | " ++ (show (tb !! 1)) ++ " | " ++ (show (tb !! 2)) ++
             "\n                              ---------------\n" ++ "                              " ++
             (show (tb !! 3)) ++ " | " ++ (show (tb !! 4)) ++ " | " ++ (show (tb !! 5)) ++
             "\n                              ---------------\n" ++ "                              " ++
             (show (tb !! 6)) ++ " | " ++ (show (tb !! 7)) ++ " | " ++ (show (tb !! 8)) ++
             "\n")
    -- verifica se o jogador1 venceu
    if (playerWin1 tb) then do
        putStrLn ("Congratulations " ++ n1 ++ "! you win!!")
-- ** passar pro ingles **
        -- abre o arquivo para escrita para atualizá-lo
        arq_escrita <- openFile "game.txt" WriteMode
        hPutStrLn arq_escrita (show (updatePontuation ps n1))
        hClose arq_escrita

        -- abre o arquivo para leitura
        arq_leitura <- openFile "game.txt" ReadMode
        dados_atualizados <- hGetLine arq_leitura
        hClose arq_leitura
        system "pause"
        --putStr "\nPressione <Enter> para voltar ao menu..."
        getChar
        menu (read dados_atualizados)
    else do
        -- verifica se o jogador2 venceu
        if (playerWin2 tb) then do
            putStrLn ("Congratulations " ++ n2 ++ "! You Win!!")

            -- abre o arquivo para escrita para atualizá-lo
            arq_escrita <- openFile "game.txt" WriteMode
            hPutStrLn arq_escrita (show (updatePontuation ps n2))
            hClose arq_escrita

            -- abre o arquivo para leitura
            arq_leitura <- openFile "game.txt" ReadMode
            dados_atualizados <- hGetLine arq_leitura
            hClose arq_leitura
            system "pause"
            --putStr "\nPressione <Enter> para voltar ao menu..."
            getChar
            menu (read dados_atualizados)
        else do
            -- verifica se houve empate
            -- se o tamanho da intersecção entre "123456789" e "table" for 0, então deu empate
            if ((length (intersect "123456789" tb)) == 0) then do
                putStrLn ("Deu empate!")
                putStr "\nPressione <Enter> para voltar ao menu..."
                getChar
                menu ps
            else do
                -- verifica se a vez é do jogador1
                if (tu == 0) then do
                    putStr (n1 ++ ", é a sua vez! Onde você quer marcar?")
                    op <- getChar
                    getChar -- descarta o Enter
                    -- testa se a opção é válida
                    if not (elem op "123456789") then do
                        putStrLn "\nEssa opção NÃO é válida, tente novamente..."
                        -- como foi opção inválida, então ainda é a vez do jogador1
                        runGame ps tb n1 n2 0
                    else
                        -- se caiu aqui, então é uma opção válida
                        -- testa se a opção já foi marcada
                        -- se ela não existir na table, é porque já foi marcada
                        if not (elem op tb) then do
                            putStrLn "\nEssa opção já foi marcada, escolha outra opção..."
                            runGame ps tb n1 n2 0
                        else
                            -- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
                            -- passa 1 para indicar que a vez é do jogador2
                            -- a nova table será o retorno da função getNewTab
                            runGame ps (getNewTab tb tu op) n1 n2 1
                else do
                    putStr (n2 ++ ", é a sua vez! Onde você quer marcar?")
                    op <- getChar
                    getChar -- descarta o Enter
                    if not (elem op "123456789") then do
                        putStrLn "\nEssa opção NÃO é válida, tente novamente..."
                        -- como foi opção inválida, então ainda é a vez do jogador2
                        runGame ps tb n1 n2 1
                    else
                        if not (elem op tb) then do
                            putStrLn "\nEssa opção já foi marcada, escolha outra opção..."
                            runGame ps tb n1 n2 1
                        else
                            -- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
                            -- passa 0 para indicar que a vez é do jogador1
                            -- a nova table será o retorno da função getNewTab
                            runGame ps (getNewTab tb tu op) n1 n2 0

start :: IO ()
start  = catch read_file handler
    where
        read_file = do
        {
            file  <- openFile "game.txt" ReadMode;
            data_ <- hGetLine file;
            hClose file;
            menu (read data_ :: Players);
            return()
        }
        handler error = if isDoesNotExistError error
        then do
        {
            file <- openFile "game.txt" WriteMode;
            hPutStrLn file "[]";
            putStrLn "Error";
            hClose file;
            menu [];
            return()
        }
        else
            ioError error

menu :: Players -> IO Players
menu data_= do
    system "cls"
    putStrLn "-------------------------------Tic_Tac_Toe--------------------------------------"
    putStrLn []
    putStrLn "1) Start the Game"
    putStrLn "2) Register a player"
    putStrLn "3) Ranking"
    putStrLn "0) Exit"
    putStr "option: "
    op <- getChar
    --getChar -- Enter
    execOpc data_ op

execOpc :: Players -> Char -> IO Players
execOpc da '1' = do
                  loadGame da
execOpc da '2' = do
                  playerReg da
execOpc da '3' = do
                 putStrLn "\nRanking:\n"
                 if (null da) then do
                    putStrLn ("** There are no registered players! **\n")
                 else
                    -- a função ordenar ordena crescentemente pela pontuação
                    listRanking (reverse (ordenar da))
                 system "pause"
                 --putStr "\nPressione <Enter> para voltar ao menu..."
                 getChar
                 menu da
execOpc da '0' = do
                  putStrLn "Bye-bye!"
                  system "cls"
                  getChar
                  return da
execOpc da  _  = do
                  putStrLn "Invalid option\n"
                  --putStr   "press <Enter> to return the menu..."
                  system "pause"
                  getChar
                  menu da

main :: IO ()
main = start

