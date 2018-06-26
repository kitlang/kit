module Main where

  import System.Console.ANSI

  print_fox = do
      setSGR [SetColor Foreground Vivid Green]
      putStr "\
        \                       ,  ,\n\
        \  _,-=._              /|_/|\n\
        \  `-.}   `=._,.-=-._.,  "
      setSGR [SetColor Foreground Vivid Cyan]
      putStr "@ @"
      setSGR [SetColor Foreground Vivid Green]
      putStrLn "._,\n\
        \     `._ _,-.   )      _,.-'\n\
        \        `    G.Q-^^u`u'"

      setSGR [SetColor Foreground Vivid Cyan]
      putStrLn "\n\
        \  oooo    oooo   o8o      ,\n\
        \  `888   .8P'    `V'    ,o8\n\
        \   888  d8'     oooo  .o888oo\n\
        \   88888[       `888    888\n\
        \   888`88b.      888    888\n\
        \   888  `88b.    888    888 ,\n\
        \  o888o  o888o  o888o   '888'"
      setSGR [Reset]

  main = do
    print_fox
