import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Verbosity
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Functor
import System.FilePath.Posix
import System.Directory
import Text.Printf

main = defaultMainWithHooks simpleUserHooks {
      hookedPrograms = [nvccProg],
      buildHook = myBuildHook 
   }
   where
     nvccProg = (simpleProgram "nvcc") {
       programPostConf = \ _ _ -> return ["--compile"]
     }

myBuildHook pkg_descr local_bld_info user_hooks bld_flags = do
  -- Extract custom fields customFieldsPD where field name x-cpp-dll-sources
  let lib       = fromJust (library pkg_descr)
      lib_bi    = libBuildInfo lib
      custom_bi = customFieldsBI lib_bi
      sources   = fromMaybe [] (lines <$> lookup "x-cuda-sources" custom_bi)
      count     = length sources
      cc_opts   = ccOptions lib_bi
      ld_opts   = ldOptions lib_bi
      inc_dirs  = includeDirs lib_bi
      lib_dirs  = extraLibDirs lib_bi
      libs      = extraLibs lib_bi
      bld_dir   = buildDir local_bld_info
      progs     = withPrograms local_bld_info
      ver       = (pkgVersion . package) pkg_descr
      nvcc      = fromJust $ lookupProgram (simpleProgram "nvcc") progs
      verb      = (fromFlag . buildVerbosity) bld_flags

  putStrLn "Compiling CUDA sources"
  objs <- mapM (\(idx,path) -> compileCuda nvcc cc_opts inc_dirs verb bld_dir idx count path) (zip [1..count] sources)

  let new_bi  = lib_bi { ldOptions = objs }
      new_lib = lib { libBuildInfo = new_bi }
      new_pkg = pkg_descr { library = Just new_lib }

  buildHook simpleUserHooks new_pkg local_bld_info user_hooks bld_flags

compileCuda :: ConfiguredProgram  -- ^ CUDA compiler (nvcc)
           -> [String]           -- ^ Compile options from Cabal
           -> [String]           -- ^ Include paths from Cabal
           -> Verbosity          -- ^ Verbosity
           -> FilePath           -- ^ Base output directory
           -> Int                -- ^ Source number
           -> Int                -- ^ Source count
           -> FilePath           -- ^ Path to source file
           -> IO FilePath        -- ^ Path to generated object code
compileCuda nvcc opts incls verb out_path idx count cxx_src = do
  let includes  = map ("-I" ++) incls
      out_path' = normalise out_path
      cxx_src'  = normalise cxx_src
      out_file  = out_path' </> dropFileName cxx_src </> replaceExtension (takeFileName cxx_src) ".o"
      out       = ["-c", cxx_src', "-o", out_file]
  createDirectoryIfMissing True (dropFileName out_file)
  let format = "[%" ++ show (length $ show count) ++ "d of " ++ show count ++"] Compiling CUDA kernel ( %s, %s )\n"
  printf format idx cxx_src out_file
  runProgram verb nvcc (includes ++ opts ++ out)
  return out_file
