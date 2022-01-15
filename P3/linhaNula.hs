module Main (main) where
    import System.IO (stdout,hSetBuffering,BufferMode(NoBuffering))
    import Data.Char
    
    main :: IO ()
    main = do hSetBuffering stdout NoBuffering
              putStr "Insira uma linha:"
              l <- getLine
              if null l
              then return ()
              else do
                    let u = map toUpper l
                    putStrLn u
                    main