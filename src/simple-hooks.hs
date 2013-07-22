import Options.Applicative

main :: IO ()
main = do
  mode <- execParser opts
  putStrLn $ "mode: " ++ show mode



data Mode = Install Bool -- ^ force
          | Run
  deriving (Show, Eq)




opts :: ParserInfo Mode
opts = info (helper <*> subcommands)
    ( fullDesc
    <> header "simple-hooks - simplify git hooks" )
  where subcommands = subparser
            ( command "install" (info
                (Install <$> switch
                              ( short 'f'
                              <> long "force"
                              <> help "Overwrite existing hooks" ))
                (progDesc "Install hooks in current repository"))
            <> command "pre-commit" ( info ( pure Run ) 
                (progDesc "Run pre-commit hooks") ) )

