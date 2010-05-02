\documentclass[9pt]{beamer}

\usepackage{color}
\definecolor{rood} {rgb}{0.8, 0.0, 0.0}
\definecolor{groen}{rgb}{0.0, 0.6, 0.0}
\definecolor{blauw}{rgb}{0.0, 0.0, 0.8}
\definecolor{grijs}{rgb}{0.5, 0.5, 0.5}
\definecolor{geel} {rgb}{0.5, 0.5, 0.0}


%include polycode.fmt

%format (ROOD (x))  = "\textcolor{rood}{" x "}"
%format (GROEN (x)) = "\textcolor{groen}{" x "}"
%format (BLAUW (x)) = "\textcolor{blauw}{" x "}"
%format (GRIJS (x)) = "\textcolor{grijs}{" x "}"
%format (GEEL (x))  = "\textcolor{geel}{" x "}"

\usepackage{alltt}

\usepackage[overlay,absolute]{textpos}
\setlength{\TPHorizModule}{10mm}
\setlength{\TPVertModule}{\TPHorizModule}

\mode<presentation>
{
  \usetheme{Warsaw}
  % or ...

  \setbeamercovered{transparent}
  % or whatever (possibly just delete it)
}

\usepackage[english]{babel}
% or whatever

\usepackage[latin1]{inputenc}
% or whatever

\usepackage{times}
\usepackage[T1]{fontenc}
% Or whatever. Note that the encoding and the font should match. If T1
% does not look nice, try deleting the line with the fontenc.


\title{Lightweight Monadic Regions}

\subtitle{Safely using scarce resources}

\author{Bas van Dijk\\\texttt{v.dijk.bas@@gmail.com}}

\date{Dutch HUG Day, 24 April 2010}

% \subject{Theoretical Computer Science}
% This is only inserted into the PDF information catalog. Can be left
% out.

% If you have a file called "university-logo-filename.xxx", where xxx
% is a graphic format that can be processed by latex or pdflatex,
% resp., then you can add a logo as follows:

% \pgfdeclareimage[height=0.5cm]{university-logo}{university-logo-filename}
% \logo{\pgfuseimage{university-logo}}

% If you wish to uncover everything in a step-wise fashion, uncomment
% the following command:
\beamerdefaultoverlayspecification{<+->}


\begin{document}

\begin{frame}
  \titlepage
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Background}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}{Background}
  \begin{itemize}
  \item Invented by: \textbf{Oleg Kiselyov \& Chung-chieh Shan}
  \item \texttt{cabal install \textbf{regions}}
  \end{itemize}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Guiding Example}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}{Guiding Example}
\framesubtitle{Specification}
  \begin{enumerate}
  \item open two files for reading, one of them a configuration file;
  \item read the name of an output file (such as the log file) from the configuration file;
  \item open the output file and zip the contents of both input files into the output file;
  \item close the configuration file;
  \item copy the rest, if any, of the other input file to the output file;
  \item close both the output and the input file.
  \end{enumerate}
\end{frame}

\begin{frame}{Guiding Example}
\framesubtitle{Example run}
  \begin{columns}[t]
    \begin{column}{.3\textwidth}
      \begin{block}{input.txt}
\begin{verbatim}
a
b
c
d
e
f
g
h
i
j
k
\end{verbatim}
      \end{block}
    \end{column}
    \begin{column}{.3\textwidth}
      \begin{block}{config.txt}
\begin{alltt}
\textcolor{blauw}{out.txt}\\
\textcolor{red}{
1\\
2\\
3\\
4}
\end{alltt}
      \end{block}
    \end{column}
    \begin{column}{.3\textwidth}
      \begin{block}{out.txt}
\begin{alltt}
\textcolor{red}{1}\\
a\\
\textcolor{red}{2}\\
b\\
\textcolor{red}{3}\\
c\\
\textcolor{red}{4}\\
d\\
e\\
f\\
g\\
h\\
i\\
j\\
k
\end{alltt}
      \end{block}
    \end{column}
  \end{columns}
\end{frame}

\begin{frame}{Guiding Example}
\framesubtitle{Implementation}
\begin{code}
test :: IO ()
test = do
  hIn <- openFile "input.txt" ReadMode
  hOut <- test_internal hIn
  till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal :: Handle -> IO Handle
test_internal hIn = do
  hCfg <- openFile "config.txt" ReadMode
  fname <- hGetLine hCfg
  hOut <- openFile fname WriteMode
  till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
    hGetLine hCfg  >>= hPutStrLn hOut
    hGetLine hIn   >>= hPutStrLn hOut
  return hOut
\end{code}

\begin{textblock}{}(6.2,1.8)
{\color{grijs}
\begin{code}
till :: Monad m => m Bool -> m () -> m ()
till condition iteration = loop
    where
      loop = do  b <- condition
                 if b
                   then return ()
                   else iteration >> loop
\end{code}}
\end{textblock}
\end{frame}

\begin{frame}{Guiding Example}
\framesubtitle{Implementation (with close)}
\begin{code}
test :: IO ()
test = do
  hIn <- openFile "input.txt" ReadMode
  hOut <- test_internal hIn
  till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut
  (ROOD(hClose hOut))
  (ROOD(hClose hIn))

test_internal :: Handle -> IO Handle
test_internal hIn = do
  hCfg <- openFile "config.txt" ReadMode
  fname <- hGetLine hCfg
  hOut <- openFile fname WriteMode
  till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
    hGetLine hCfg  >>= hPutStrLn hOut
    hGetLine hIn   >>= hPutStrLn hOut
  (ROOD(hClose hCfg))
  return hOut
\end{code}
\end{frame}

\begin{frame}{Guiding Example}
\framesubtitle{Implementation (with exception handling)}
\begin{code}
test :: IO ()
test =
  (ROOD(bracket (openFile "input.txt" ReadMode) hClose)) $ \hIn ->
    (ROOD(bracket (test_internal hIn) hClose)) $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal :: Handle -> IO Handle
test_internal hIn = do
  (ROOD(bracket (openFile "config.txt" ReadMode) hClose)) $ \hCfg -> do
    fname <- hGetLine hCfg
    (ROOD(bracketOnError (openFile fname WriteMode) hClose)) $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
        hGetLine hCfg  >>= hPutStrLn hOut
        hGetLine hIn   >>= hPutStrLn hOut
      return hOut
\end{code}
\end{frame}

\begin{frame}{Guiding Example}
\framesubtitle{Implementation}
\begin{code}
test :: IO ()
test =
  bracket (openFile "input.txt" ReadMode) hClose $ \hIn ->
    bracket (test_internal hIn) hClose $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal :: Handle -> IO Handle
test_internal hIn = do
  bracket (openFile "config.txt" ReadMode) hClose $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError (openFile fname WriteMode) hClose $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
        hGetLine hCfg  >>= hPutStrLn hOut
        hGetLine hIn   >>= hPutStrLn hOut
      return hOut
\end{code}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Explicit IOModes}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}{IOModes}
\framesubtitle{Only read from read-only handles!}
\begin{code}
test :: IO ()
test =
  bracket (openFile "input.txt" (ROOD(ReadMode))) hClose $ \hIn ->
    bracket (test_internal hIn) hClose $ \hOut ->
      till (hIsEOF hIn) $ (ROOD(hGetLine hIn)) >>= hPutStrLn hOut

test_internal :: Handle -> IO Handle
test_internal hIn = do
  bracket (openFile "config.txt" ReadMode) hClose $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError (openFile fname WriteMode) hClose $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) ((ROOD(hIsEOF hIn)))) $ do
        hGetLine hCfg         >>= hPutStrLn hOut
        (ROOD(hGetLine hIn))  >>= hPutStrLn hOut
      return hOut
\end{code}
\end{frame}

\begin{frame}{IOModes}
\framesubtitle{Only write to write-only handles!}
\begin{code}
test :: IO ()
test =
  bracket (openFile "input.txt" ReadMode) hClose $ \hIn ->
    bracket (test_internal hIn) hClose $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= (BLAUW(hPutStrLn hOut))

test_internal :: Handle -> IO Handle
test_internal hIn = do
  bracket (openFile "config.txt" ReadMode) hClose $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError (openFile fname (BLAUW(WriteMode))) hClose $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
        hGetLine hCfg  >>= (BLAUW(hPutStrLn hOut))
        hGetLine hIn   >>= (BLAUW(hPutStrLn hOut))
      return hOut
\end{code}
\end{frame}

\begin{frame}{IOModes}
\framesubtitle{Encode IOModes in types}
\begin{itemize}
\item \texttt{cabal install \textbf{explicit-iomodes}}
\item |newtype Handle (ROOD(ioMode)) = Handle (System.IO.Handle)|
\item
\begin{code}
data ReadMode
data WriteMode
data AppendMode
data ReadWriteMode
\end{code}
\item |openFile :: FilePath -> IOMode (ROOD(ioMode)) -> IO (Handle (ROOD(ioMode)))|
\item
\begin{code}
data IOMode ioMode where
    ReadMode       :: IOMode (ROOD(ReadMode))
    WriteMode      :: IOMode (ROOD(WriteMode))
    AppendMode     :: IOMode (ROOD(AppendMode))
    ReadWriteMode  :: IOMode (ROOD(ReadWriteMode))
\end{code}
\end{itemize}
\end{frame}

\begin{frame}{IOModes}
\framesubtitle{Constrain operations}
\begin{itemize}
\item
\begin{code}
hGetLine   :: (ROOD(ReadModes))   (ROOD(ioMode =>)) Handle (ROOD(ioMode)) -> IO String
hIsEOF     :: (ROOD(ReadModes))   (ROOD(ioMode =>)) Handle (ROOD(ioMode)) -> IO Bool
hPutStrLn  :: (ROOD(WriteModes))  (ROOD(ioMode =>)) Handle (ROOD(ioMode)) -> String -> IO ()
\end{code}
\item
\begin{code}
class ReadModes   ioMode
class WriteModes  ioMode

instance ReadModes   ReadMode
instance ReadModes   ReadWriteMode

instance WriteModes  WriteMode
instance WriteModes  AppendMode
instance WriteModes  ReadWriteMode
\end{code}
\end{itemize}
\end{frame}

\begin{frame}{IOModes}
\framesubtitle{Only one import!}
\begin{code}
import System.IO.ExplicitIOModes

test :: IO ()
test =
  bracket (openFile "input.txt" ReadMode) hClose $ \hIn ->
    bracket (test_internal hIn) hClose $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal ::  ReadModes ioMode =>
                  Handle ioMode -> IO (Handle WriteMode)
test_internal hIn = do
  bracket (openFile "config.txt" ReadMode) hClose $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError (openFile fname WriteMode) hClose $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
        hGetLine hCfg  >>= hPutStrLn hOut
        hGetLine hIn   >>= hPutStrLn hOut
      return hOut
\end{code}
\end{frame}

\begin{frame}{IOModes}
\framesubtitle{Inferred types change (|hIn|)}
\begin{code}
import System.IO.ExplicitIOModes

test :: IO ()
test =
  bracket (openFile "input.txt" ReadMode) hClose $ \hIn ->
    bracket (test_internal hIn) hClose $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal ::  (ROOD(ReadModes ioMode)) =>
                  Handle (ROOD(ioMode)) -> IO (Handle WriteMode)
test_internal (ROOD(hIn)) = do
  bracket (openFile "config.txt" ReadMode) hClose $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError (openFile fname WriteMode) hClose $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) ((ROOD(hIsEOF hIn)))) $ do
        hGetLine hCfg         >>= hPutStrLn hOut
        (ROOD(hGetLine hIn))  >>= hPutStrLn hOut
      return hOut
\end{code}
\end{frame}

\begin{frame}{IOModes}
\framesubtitle{Inferred types change (|hOut|)}
\begin{code}
import System.IO.ExplicitIOModes

test :: IO ()
test =
  bracket (openFile "input.txt" ReadMode) hClose $ \hIn ->
    bracket (test_internal hIn) hClose $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal ::  (ROOD(ReadModes ioMode)) =>
                  Handle (ROOD(ioMode)) -> IO (Handle (BLAUW(WriteMode)))
test_internal (ROOD(hIn)) = do
  bracket (openFile "config.txt" ReadMode) hClose $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError ((BLAUW(openFile fname WriteMode))) hClose $ \(BLAUW(hOut)) -> do
      till (liftM2 (||) (hIsEOF hCfg) ((ROOD(hIsEOF hIn)))) $ do
        hGetLine hCfg         >>= (BLAUW(hPutStrLn hOut))
        (ROOD(hGetLine hIn))  >>= (BLAUW(hPutStrLn hOut))
      return (BLAUW(hOut))
\end{code}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Regions}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Close automatically}

\begin{frame}{Regions}
\framesubtitle{Don't forget to close!}
\begin{code}
test :: IO ()
test =
  bracket ((GROEN(openFile)) "input.txt" ReadMode) (ROOD(hClose)) $ \hIn ->
    bracket (test_internal hIn) (ROOD(hClose)) $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal ::  ReadModes ioMode =>
                  Handle ioMode -> IO (Handle WriteMode)
test_internal hIn = do
  bracket ((GROEN(openFile)) "config.txt" ReadMode) (ROOD(hClose)) $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError ((GROEN(openFile)) fname WriteMode) (ROOD(hClose)) $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
        hGetLine hCfg  >>= hPutStrLn hOut
        hGetLine hIn   >>= hPutStrLn hOut
      return hOut
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{|withFile|}
\begin{code}
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp ioMode = bracket ((GROEN(openFile)) fp ioMode) (ROOD(hClose)
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Use |withFile| to abstract close}
\begin{code}
test :: IO ()
test =
  (BLAUW(withFile)) "input.txt" ReadMode $ \hIn ->
    bracket (test_internal hIn) (ROOD(hClose)) $ \hOut ->
      till (hIsEOF hIn) $ hGetLine hIn >>= hPutStrLn hOut

test_internal ::  ReadModes ioMode =>
                  Handle ioMode -> IO (Handle WriteMode)
test_internal hIn = do
  (BLAUW(withFile)) "config.txt" ReadMode $ \hCfg -> do
    fname <- hGetLine hCfg
    bracketOnError ((GROEN(openFile)) fname WriteMode) (ROOD(hClose)) $ \hOut -> do
      till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
        hGetLine hCfg  >>= hPutStrLn hOut
        hGetLine hIn   >>= hPutStrLn hOut
      return hOut
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Extend |openFile|}
\framesubtitle{}
\begin{code}
openFile :: FilePath -> IOMode ioMode -> IO (E.Handle ioMode)
\end{code}
\end{frame}

\begin{frame}{Regions}
\begin{itemize}
\item
\begin{code}
openFile ::  (ROOD(MonadCatchIO pr =>))
             FilePath -> IOMode ioMode -> (ROOD(RegionT pr)) (E.Handle ioMode)
\end{code}
\item
%{
%format forall = "\forall"
%format . = ".\ "
\begin{code}
newtype RegionT pr a = RegionT
    {unRegionT :: ReaderT (IORef [AnyHandle]) pr a}
\end{code}
\item
\begin{code}
    deriving  (  Functor
              ,  Applicative
              ,  Alternative
              ,  Monad
              ,  MonadPlus
              ,  MonadFix
              ,  MonadTrans
              ,  MonadIO
              ,  MonadCatchIO
              )
\end{code}
\item
\begin{code}
data AnyHandle = forall ioMode. Any (Handle ioMode)
\end{code}
%}
\end{itemize}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Implementation of |openFile|}
\begin{code}
openFile ::  MonadCatchIO pr =>
             FilePath -> IOMode ioMode -> RegionT pr (E.Handle ioMode)
openFile fp ioMode = block $ do
  h <- liftIO $ E.openFile fp ioMode
  register h
  return h

register :: MonadCatchIO pr => Handle ioMode -> RegionT pr ()
register h = RegionT $ ask >>= liftIO . flip modifyIORef (Any h:)
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Call native |openFile|}
\begin{code}
openFile ::  MonadCatchIO pr =>
             FilePath -> IOMode ioMode -> RegionT pr (E.Handle ioMode)
openFile fp ioMode = block $ do
  h <- liftIO $ (ROOD(E.openFile)) fp ioMode
  register h
  return h

register :: MonadCatchIO pr => Handle ioMode -> RegionT pr ()
register h = RegionT $ ask >>= liftIO . flip modifyIORef (Any h:)
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{|register| handle}
\begin{code}
openFile ::  MonadCatchIO pr =>
             FilePath -> IOMode ioMode -> RegionT pr (E.Handle ioMode)
openFile fp ioMode = block $ do
  h <- liftIO $ (ROOD(E.openFile)) fp ioMode
  (BLAUW(register h))
  return h

register :: MonadCatchIO pr => Handle ioMode -> RegionT pr ()
register h = RegionT $ ask >>= liftIO . flip modifyIORef (Any h:)
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{|register| handle}
\begin{code}
openFile ::  MonadCatchIO pr =>
             FilePath -> IOMode ioMode -> RegionT pr (E.Handle ioMode)
openFile fp ioMode = block $ do
  h <- liftIO $ (ROOD(E.openFile)) fp ioMode
  (BLAUW(register h))
  return h

(BLAUW(register :: MonadCatchIO pr => Handle ioMode -> RegionT pr ()))
(BLAUW(register h = RegionT $ ask >>= liftIO . flip modifyIORef (Any h:)))
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{|block| asynchronous exceptions}
\begin{code}
openFile ::  MonadCatchIO pr =>
             FilePath -> IOMode ioMode -> RegionT pr (E.Handle ioMode)
openFile fp ioMode = (GROEN(block)) $ do
  h <- liftIO $ (ROOD(E.openFile)) fp ioMode
  (BLAUW(register h))
  return h

(BLAUW(register :: MonadCatchIO pr => Handle ioMode -> RegionT pr ()))
(BLAUW(register h = RegionT $ ask >>= liftIO . flip modifyIORef (Any h:)))
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Running regions}
\begin{code}
runRegionT :: MonadCatchIO pr => RegionT pr a -> pr a
runRegionT r = bracket   (liftIO $ newIORef [])
                         (liftIO . after)
                         (runReaderT $ unRegionT r)
    where
      after ioref = do  hs <- readIORef ioref
                        forM_ hs $ \(Any h) -> hClose h
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Create initial empty list of handles}
\begin{code}
runRegionT :: MonadCatchIO pr => RegionT pr a -> pr a
runRegionT r = bracket   (liftIO $ (ROOD(newIORef [])))
                         (liftIO . after)
                         (runReaderT $ unRegionT r)
    where
      after ioref = do  hs <- readIORef ioref
                        forM_ hs $ \(Any h) -> hClose h
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Run given region}
\begin{code}
runRegionT :: MonadCatchIO pr => RegionT pr a -> pr a
runRegionT r = bracket   (liftIO $ (ROOD(newIORef [])))
                         (liftIO . after)
                         ((GROEN(runReaderT $ unRegionT r)))
    where
      after ioref = do  hs <- readIORef ioref
                        forM_ hs $ \(Any h) -> hClose h
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Call finalizer}
\begin{code}
runRegionT :: MonadCatchIO pr => RegionT pr a -> pr a
runRegionT r = bracket   (liftIO $ (ROOD(newIORef [])))
                         (liftIO . (BLAUW(after)))
                         ((GROEN(runReaderT $ unRegionT r)))
    where
      after ioref = do    hs <- readIORef ioref
                          forM_ hs $ \(Any h) -> hClose h
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Close all opened handles}
\begin{code}
runRegionT :: MonadCatchIO pr => RegionT pr a -> pr a
runRegionT r = bracket   (liftIO $ (ROOD(newIORef [])))
                         (liftIO . (BLAUW(after)))
                         ((GROEN(runReaderT $ unRegionT r)))
    where
      BLAUW(after ioref = do)  (BLAUW(hs <- readIORef ioref))
                               (BLAUW(forM_ hs $ \(Any h) -> hClose h))
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Operations}
\begin{code}
hGetLine ::  (ReadModes ioMode, MonadIO r) =>
             Handle ioMode -> r String
hGetLine h = liftIO $ E.hGetLine h
\end{code}
\begin{code}
hIsEOF ::  (ReadModes ioMode, MonadIO r) =>
           Handle ioMode -> r Bool
hIsEOF h = liftIO $ E.hIsEOF h
\end{code}
\begin{code}
hPutStrLn ::  (WriteModes ioMode, MonadIO r) =>
              Handle ioMode -> String -> r ()
hPutStrLn h s = liftIO $ E.hPutStrLn h s
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Operations - Just lift them}
\begin{code}
hGetLine ::  (ReadModes ioMode, (ROOD(MonadIO r))) =>
             Handle ioMode -> (ROOD(r)) String
hGetLine h = (ROOD(liftIO)) $ E.hGetLine h
\end{code}
\begin{code}
hIsEOF ::  (ReadModes ioMode, (ROOD(MonadIO r))) =>
           Handle ioMode -> (ROOD(r)) Bool
hIsEOF h = (ROOD(liftIO)) $ E.hIsEOF h
\end{code}
\begin{code}
hPutStrLn ::  (WriteModes ioMode, (ROOD(MonadIO r))) =>
              Handle ioMode -> String -> (ROOD(r)) ()
hPutStrLn h s = (ROOD(liftIO)) $ E.hPutStrLn h s
\end{code}
\end{frame}

\begin{frame}{Regions}
\begin{code}
(GRIJS(main :: IO ()))
(GRIJS(main = runRegionT test))

test :: MonadCatchIO pr => RegionT pr ()
test = do
  hIn <- openFile "input.txt" ReadMode
  hOut <- runRegionT $ test_internal hIn
  till (hIsEOF hIn) $
    hGetLine hIn >>= hPutStrLn hOut

test_internal  :: (MonadCatchIO pr, ReadModes ioMode)
               => Handle ioMode -> RegionT (RegionT pr) (Handle WriteMode)
test_internal hIn = do
  hCfg <- openFile "config.txt" ReadMode
  fname <- hGetLine hCfg
  hOut <- lift $ openFile fname WriteMode
  till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
    hGetLine hCfg >>= hPutStrLn hOut
    hGetLine hIn  >>= hPutStrLn hOut
  return hOut
\end{code}
\end{frame}

\begin{frame}{Regions}
\begin{code}
(GRIJS(main :: IO ()))
(GRIJS(main = runRegionT test))

test :: MonadCatchIO pr => RegionT pr ()
test = do
  hIn <- (GROEN(openFile "input.txt" ReadMode))
  hOut <- runRegionT $ test_internal hIn
  till (hIsEOF hIn) $
    hGetLine hIn >>= hPutStrLn hOut

test_internal  :: (MonadCatchIO pr, ReadModes ioMode)
               => Handle ioMode -> RegionT (RegionT pr) (Handle WriteMode)
test_internal hIn = do
  hCfg <- (GROEN(openFile "config.txt" ReadMode))
  fname <- hGetLine hCfg
  hOut <- lift $ (GROEN(openFile fname WriteMode))
  till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
    hGetLine hCfg >>= hPutStrLn hOut
    hGetLine hIn  >>= hPutStrLn hOut
  return hOut
\end{code}
\end{frame}

\begin{frame}{Regions}
\begin{code}
(GRIJS(main :: IO ()))
(GRIJS(main = runRegionT test))

test :: (BLAUW(MonadCatchIO pr => RegionT pr ()))
test = do
  hIn <- (GROEN(openFile "input.txt" ReadMode))
  hOut <- runRegionT $ test_internal hIn
  till (hIsEOF hIn) $
    hGetLine hIn >>= hPutStrLn hOut

test_internal  :: ((BLAUW(MonadCatchIO pr)), ReadModes ioMode)
               => Handle ioMode -> (BLAUW(RegionT (RegionT pr))) (Handle WriteMode)
test_internal hIn = do
  hCfg <- (GROEN(openFile "config.txt" ReadMode))
  fname <- hGetLine hCfg
  hOut <- lift $ (GROEN(openFile fname WriteMode))
  till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
    hGetLine hCfg >>= hPutStrLn hOut
    hGetLine hIn  >>= hPutStrLn hOut
  return hOut
\end{code}
\end{frame}

\begin{frame}{Regions}
\begin{code}
(GRIJS(main :: IO ()))
(GRIJS(main = (GEEL(runRegionT)) test))

test :: (BLAUW(MonadCatchIO pr => RegionT pr ()))
test = do
  hIn <- (GROEN(openFile "input.txt" ReadMode))
  hOut <- (GEEL(runRegionT)) $ test_internal hIn
  till (hIsEOF hIn) $
    hGetLine hIn >>= hPutStrLn hOut

test_internal  :: ((BLAUW(MonadCatchIO pr)), ReadModes ioMode)
               => Handle ioMode -> (BLAUW(RegionT (RegionT pr))) (Handle WriteMode)
test_internal hIn = do
  hCfg <- (GROEN(openFile "config.txt" ReadMode))
  fname <- hGetLine hCfg
  hOut <- lift $ (GROEN(openFile fname WriteMode))
  till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
    hGetLine hCfg >>= hPutStrLn hOut
    hGetLine hIn  >>= hPutStrLn hOut
  return hOut
\end{code}
\end{frame}

\begin{frame}{Regions}
\begin{code}
(GRIJS(main :: IO ()))
(GRIJS(main = (GEEL(runRegionT)) test))

test :: (BLAUW(MonadCatchIO pr => RegionT pr ()))
test = do
  hIn <- (GROEN(openFile "input.txt" ReadMode))
  hOut <- (GEEL(runRegionT)) $ test_internal hIn
  till (hIsEOF hIn) $
    hGetLine hIn >>= hPutStrLn hOut

test_internal  :: ((BLAUW(MonadCatchIO pr)), ReadModes ioMode)
               => Handle ioMode -> (BLAUW(RegionT (RegionT pr))) (Handle WriteMode)
test_internal hIn = do
  hCfg <- (GROEN(openFile "config.txt" ReadMode))
  fname <- hGetLine hCfg
  hOut <- (ROOD(lift)) $ (GROEN(openFile fname WriteMode))
  till (liftM2 (||) (hIsEOF hCfg) (hIsEOF hIn)) $ do
    hGetLine hCfg >>= hPutStrLn hOut
    hGetLine hIn  >>= hPutStrLn hOut
  return hOut
\end{code}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Scoping}

\begin{frame}{Regions}
\framesubtitle{Returning handles from regions}
%{
%format forall = "\forall"
%format . = ".\ "
\begin{code}
hack :: forall pr. MonadCatchIO pr => pr ()
hack = do  h <- runRegionT region
           (ROOD(hPutStrLn h "Don't do this at home!"))
  where
    region :: RegionT pr ((ROOD(Handle WriteMode)))
    region = do  h <- openFile "output.txt" WriteMode
                 (ROOD(return h))
\end{code}
%}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Associate handle with region}
\begin{itemize}
\item
\begin{code}
newtype RegionalHandle ioMode (ROOD((r :: * -> *))) = RegionalHandle
    {internalHandle :: Handle ioMode}
\end{code}
\item
\begin{code}
openFile ::  MonadCatchIO pr =>
             FilePath -> IOMode ioMode ->
               RegionT (ROOD(s)) pr (ROOD((RegionalHandle ioMode (RegionT s pr))))
\end{code}
\end{itemize}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Restrict handles to their region}
\begin{itemize}
\item
\begin{code}
newtype RegionT (ROOD(s)) pr a = ...
\end{code}
\item
%{
%format forall = "\forall"
%format . = ".\ "
\begin{code}
runRegionT :: MonadCatchIO pr => ((ROOD(forall s.)) RegionT (ROOD(s)) pr (BLAUW(a))) -> pr (BLAUW(a))
\end{code}
\item
\begin{code}
hack :: forall pr. MonadCatchIO pr => pr ()
hack = do  h <- runRegionT region
           hPutStrLn h "Don't do this at home!"
  where
    region :: RegionT (ROOD(s)) pr ((BLAUW(RegionalHandle WriteMode (RegionT (ROOD(s)) pr))))
    region = do  h <- openFile "output.txt" WriteMode
                 return h
\end{code}
%}
\item \texttt{Inferred type is less polymorphic than expected.\\
      Quantified type variable `\textcolor{rood}{s}' escapes}
\end{itemize}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Operations}
\begin{code}
hGetLine ::  (ReadModes ioMode , MonadIO cr) =>
             (ROOD(RegionalHandle)) ioMode (ROOD(pr)) -> cr String
hGetLine h = liftIO $ E.hGetLine ((BLAUW(internalHandle)) h)
\end{code}
\begin{code}
hIsEOF ::  (ReadModes ioMode, MonadIO cr) =>
           (ROOD(RegionalHandle)) ioMode (ROOD(pr)) -> cr Bool
hIsEOF h = liftIO $ E.hIsEOF ((BLAUW(internalHandle)) h)
\end{code}
\begin{code}
hPutStrLn ::  (WriteModes ioMode, MonadIO cr) =>
              (ROOD(RegionalHandle)) ioMode (ROOD(pr)) -> String -> cr ()
hPutStrLn h s = liftIO $ E.hPutStrLn ((BLAUW(internalHandle)) h) s
\end{code}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{ParentOf}

\begin{frame}{Regions}
\framesubtitle{Return computation}
\begin{itemize}
\item
%{
%format forall = "\forall"
%format . = ".\ "
\begin{code}
hack :: forall pr. MonadCatchIO pr => pr ()
hack = do  h <- runRegionT region
           hPutStrLn h "Don't do this at home!"
  where
    region :: RegionT (ROOD(s)) pr ((BLAUW(RegionalHandle WriteMode (RegionT (ROOD(s)) pr))))
    region = do  h <- openFile "output.txt" WriteMode
                 return h
\end{code}
\item
\begin{code}
hack2 :: forall pr. MonadCatchIO pr => pr ()
hack2 = do  (ROOD(m)) <- runRegionT region
            (ROOD(m))
  where
    region :: RegionT s pr ((ROOD(pr ())))
    region = do  h <- openFile "output.txt" WriteMode
                 return $ (ROOD(hPutStrLn h "Don't do this at home!"))
\end{code}
%}
\end{itemize}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Operations are too general!}
\begin{code}
hGetLine ::  (ReadModes ioMode , MonadIO cr) =>
             RegionalHandle ioMode (ROOD(pr)) -> (ROOD(cr)) String
\end{code}
\begin{code}
hIsEOF ::  (ReadModes ioMode, MonadIO cr) =>
           RegionalHandle ioMode (ROOD(pr)) -> (ROOD(cr)) Bool
\end{code}
\begin{code}
hPutStrLn ::  (WriteModes ioMode, MonadIO cr) =>
              RegionalHandle ioMode (ROOD(pr)) -> String -> (ROOD(cr)) ()
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Restrict}
\begin{code}
hGetLine ::  (ReadModes ioMode, MonadIO cr, (ROOD(pr `ParentOf` cr))) =>
             RegionalHandle ioMode (ROOD(pr)) -> (ROOD(cr)) String
\end{code}
\begin{code}
hIsEOF ::  (ReadModes ioMode, MonadIO cr, (ROOD(pr `ParentOf` cr))) =>
           RegionalHandle ioMode (ROOD(pr)) -> (ROOD(cr)) Bool
\end{code}
\begin{code}
hPutStrLn ::  (WriteModes ioMode, MonadIO cr, (ROOD(pr `ParentOf` cr))) =>
              RegionalHandle ioMode (ROOD(pr)) -> String -> (ROOD(cr)) ()
\end{code}
\end{frame}


\begin{frame}{Regions}
\framesubtitle{|ParentOf|}
\begin{itemize}
\item
\begin{code}
class (Monad pr, Monad cr) => pr `ParentOf` cr
\end{code}
\item
\begin{code}
instance Monad r => r `ParentOf` r
\end{code}
\item
\begin{code}
instance  ( Monad cr
          , cr (ROOD(`TypeCast2`)) RegionT s pcr
          , pr `ParentOf` pcr
          )
          => pr `ParentOf` cr
\end{code}
\item
\begin{code}
(GROEN(RegionT ps ppr)) `ParentOf`  RegionT cs
                                      (RegionT pcs
                                        (RegionT ppcs
                                          ...
                                            ((GROEN(RegionT ps ppr)))))
\end{code}
\end{itemize}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Type casting}
\begin{code}
instance  ( Monad cr
          , cr (ROOD(`TypeCast2`)) RegionT s pcr
          , pr `ParentOf` pcr
          )
          => pr `ParentOf` cr
\end{code}
\begin{code}
class TypeCast2       (a :: * -> *) (b :: * -> *)  |     a -> b
                                                   ,     b -> a
class TypeCast2'   t  (a :: * -> *) (b :: * -> *)  |  t  a -> b
                                                   ,  t  b -> a
class TypeCast2''  t  (a :: * -> *) (b :: * -> *)  |  t  a -> b
                                                   ,  t  b -> a

instance TypeCast2'    ()  a b => TypeCast2     a b
instance TypeCast2''   t   a b => TypeCast2' t  a b
instance TypeCast2''   ()  a a
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{|hack2| now fails}
\begin{itemize}
\item
%{
%format forall = "\forall"
%format . = ".\ "
\begin{code}
hack2 :: forall sp pr. MonadCatchIO pr => RegionT sp pr ()
hack2 = do  m <- runRegionT region
            m
  where
    region :: forall (ROOD(s)). RegionT (ROOD(s)) (RegionT sp pr) (RegionT sp pr ())
    region = do  h <- openFile "output.txt" WriteMode
                 return $ hPutStrLn h "Don't do this at home!"
\end{code}
%}
\item
\texttt{Inferred type is less polymorphic than expected\\
        Quantified type variable `\textcolor{rood}{s}' is mentioned in the environment:...}
 \item |RegionT (ROOD(s)) (RegionT sp pr) `ParentOf` (RegionT sp pr)|
\end{itemize}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Leaking Handles via IOError}

\begin{frame}{Regions}
\framesubtitle{Leaking handles via |IOErrors|}
\begin{itemize}
\item
\begin{code}
throwsException = runRegionT $ do
                    h <- openFile "foo.txt" ReadMode
                    hSeek h AbsoluteSeek ((ROOD(-1)))
\end{code}
\item
\begin{code}
leak =  throwsException `catch` \(e :: IOError) ->
          case (ROOD(ioeGetHandle e)) of
            Nothing  -> return ()
            Just h   -> System.IO.hGetLine h
\end{code}
\item
\begin{code}
ioeGetHandle :: IOError -> Maybe (ROOD(Handle))
\end{code}
\end{itemize}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Remove handles from |IOErrors|}
\begin{itemize}
\item
\begin{code}
sanitize :: IO a -> IO a
sanitize = modifyIOError $ \e -> e { ioe_handle = Nothing }
\end{code}
\item
\begin{code}
hIsEOF     h       = liftIO $ (ROOD(sanitize)) $ E.hIsEOF     (internalHandle h)
hPutStrLn  h s     = liftIO $ (ROOD(sanitize)) $ E.hPutStrLn  (internalHandle h) s
hSeek      h sm n  = liftIO $ (ROOD(sanitize)) $ E.hSeek      (internalHandle h) sm n
\end{code}
\end{itemize}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Extension: handle duplication}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{frame}{Regions}
\framesubtitle{Extension: dynamically extending the lifetime of handles}
\begin{code}
test2 = runRegionT $ do
  h <- runRegionT $ test5_internal "conf2.txt"
  l <- hGetLine h

test2_internal conf_fname = do
  hc <- openFile conf_fname ReadMode
  fname1 <- hGetLine hc
  fname2 <- hGetLine hc
  h1 <- openFile fname1 ReadMode
  h2 <- openFile fname2 ReadMode
  l1 <- hGetLine h1
  l2 <- hGetLine h2
  let hOld = if l1 < l2 then h2 else h1
  return hOld
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Extension: dynamically extending the lifetime of handles}
\begin{code}
test2 = runRegionT $ do
  h <- runRegionT $ test5_internal "conf2.txt"
  l <- hGetLine h

test2_internal conf_fname = do
  hc <- openFile conf_fname ReadMode
  fname1 <- hGetLine hc
  fname2 <- hGetLine hc
  h1 <- openFile fname1 ReadMode
  h2 <- openFile fname2 ReadMode
  l1 <- hGetLine h1
  l2 <- hGetLine h2
  let hOld = if l1 < l2 then h2 else h1
  (ROOD(return hOld))
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Extension: dynamically extending the lifetime of handles}
\begin{code}
test2 = runRegionT $ do
  h <- runRegionT $ test5_internal "conf2.txt"
  l <- hGetLine h

test2_internal conf_fname = do
  hc <- openFile conf_fname ReadMode
  fname1 <- hGetLine hc
  fname2 <- hGetLine hc
  h1 <- openFile fname1 ReadMode
  h2 <- openFile fname2 ReadMode
  l1 <- hGetLine h1
  l2 <- hGetLine h2
  let hOld = if l1 < l2 then h2 else h1
  (ROOD(dup hOld))
\end{code}
\end{frame}

\begin{frame}{Regions}
\framesubtitle{Extension: dynamically extending the lifetime of handles}
\begin{code}
dup ::  MonadCatchIO ppr =>
        RegionalHandle ioMode ((ROOD(RegionT cs))) ((BLAUW(RegionT ps ppr)))) ->
          (ROOD(RegionT cs)) ((BLAUW(RegionT ps ppr)))
            (RegionalHandle ioMode ((BLAUW(RegionT ps ppr))))
\end{code}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Generalization}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Scarce resources}

\begin{frame}{Generalization}
\framesubtitle{Not just files}
\begin{code}
class Resource resource where
    data Handle resource :: *

    openResource   :: resource -> IO (Handle resource)
    closeResource  :: Handle resource -> IO ()
\end{code}

\pause

\begin{code}
data RegionalHandle (ROOD(resource)) (r :: * -> *) = RegionalHandle
    { internalHandle :: (ROOD(Handle resource)) }
\end{code}

\pause

\begin{code}
open ::  ((ROOD(Resource resource)), MonadCatchIO m) =>
         (ROOD(resource)) ->   RegionT s m
                                 (RegionalHandle (ROOD(resource)) (RegionT s m))
\end{code}
\end{frame}

\begin{frame}{Generalization}
\framesubtitle{Not just files}
\begin{code}
newtype RegionT s m a = RegionT
    {unRegionT :: ReaderT (IORef [AnyHandle]) m a}
\end{code}

\pause

\begin{code}
data AnyHandle =  forall (ROOD(resource)) r. (ROOD(Resource resource =>))
                  Any (RegionalHandle (ROOD(resource)) r)
\end{code}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Files}

\begin{frame}{Generalization}
\framesubtitle{Files as scarce resources}

\texttt{cabal install \textbf{safer-file-handles}}

\pause
\begin{code}
data File ioMode where
    File      ::  Binary -> FilePath -> IOMode ioMode -> File ioMode
    TempFile  ::  Binary -> FilePath -> Template -> DefaultPermissions ->
                    File ReadWriteMode
\end{code}
\end{frame}

\begin{frame}{}
\begin{code}
instance Resource (File ioMode) where
  data Handle (File ioMode) =
        FileHandle  { mbFilePath :: Maybe FilePath
                    , handle :: E.Handle ioMode }

  openResource (File isBinary filePath ioMode) =
      FileHandle Nothing <$>
        (if isBinary then E.openBinaryFile else E.openFile)
        filePath ioMode

  openResource (TempFile isBinary filePath template defaultPerms) =
      uncurry (FileHandle . Just) <$>
        (case (isBinary, defaultPerms) of
            (False,  False)  -> E.openTempFile
            (True,   False)  -> E.openBinaryTempFile
            (False,  True)   -> E.openTempFileWithDefaultPermissions
            (True,   True)   -> E.openBinaryTempFileWithDefaultPermissions)
          filePath template

  closeResource = sanitizeIOError . E.hClose . handle
\end{code}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Memory}

\begin{frame}{Generalization}
\framesubtitle{Memory as a scarce resource}

\texttt{cabal install \textbf{regional-pointers}}

\pause

\begin{code}
newtype Memory a = Memory { size :: Int }
\end{code}

\pause

\begin{code}
instance Resource (Memory a) where
    newtype Handle (Memory a) = Pointer { ptr :: Ptr a }

    openResource   = fmap Pointer . mallocBytes . size
    closeResource  = free . ptr
\end{code}

\pause

\begin{code}
type RegionalPtr a r = RegionalHandle (Memory a) r
\end{code}

\pause

\begin{code}
peek ::  (pr `ParentOf` cr, Storable a, MonadIO cr) =>
         RegionalPtr a pr -> cr a

poke ::  (pr `ParentOf` cr, Storable a, MonadIO cr) =>
         RegionalPtr a pr -> a -> cr ()
\end{code}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{USB Devices}

\begin{frame}{Generalization}
\framesubtitle{USB devices as scarce resources}
\texttt{cabal install \textbf{usb}}

\pause

\begin{code}
data Device = Device  {  getDevFrgnPtr :: ForeignPtr C'libusb_device
                      ,  ...
                      }
\end{code}

\pause

\texttt{cabal install \textbf{usb-safe}}

\pause

\begin{code}
instance Resource USB.Device where

    data Handle USB.Device = DeviceHandle
        { internalDevHndl :: USB.DeviceHandle, ... }

    openResource   = fmap DeviceHandle . USB.openDevice
    closeResource  = USB.closeDevice . internalDevHndl
\end{code}

\pause

\begin{code}
type RegionalDeviceHandle r = RegionalHandle USB.Device r
\end{code}

\end{frame}

\begin{frame}{Questions}
\begin{code}
Questions?
\end{code}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\end{document}


