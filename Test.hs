import StarPU
import DataTypes
import BLAS
import Task

main = do
  putStrLn "Initializing..."
  cublasInit
  defaultInit
  showRuntimeInfo
  putStrLn "Register matrices..."
  m1 <- floatMatrixRegisterInvalid 128 128 128
  m2 <- floatMatrixRegisterInvalid 128 128 128
  putStrLn "Performing SGEMM..."
  r <- return $! sgemm m1 m2
  putStrLn "Wait for all tasks"
  taskWaitForAll
  putStrLn "Unregister matrices..."
  unregister m1
  unregister m2
  putStrLn "Shutting down..."
  shutdown


qr_lu f11 f1k fk1 fkk i a = unsplit i i bs
  where
    as = split i i a
    (a11,ak1,a1k,akk) = triangularSplit as
    b11 = f11 a11
    bk1 = map (fk1 b11) ak1
    b1k = map (f1k b11) a1k
    tmpkk = zipWith fkk akk (cross bk1 b1k)
    bkk = if null tmpkk then tmpkk else split (i-1) (i-1) $ qr_lu f11 f1k fk1 fkk i $ unsplit (i-1) (i-1) tmpkk
    bs = fromTriangularSplit (b11,bk1,b1k,bkk)



triangularSplit m = (m11,mk1,m1k,mkk)
  where
    m11 = head (column 1 m)
    mk1 = tail (column 1 m)
    m1k = tail (row 1 m)
    mkk = dropRows 1 (dropColumns 1 m)

fromTriangularSplit (m11,mk1,m1k,mkk) = fromColumns $ left:right
  where
    left = m11:mk1
    right = columns $ fromRows $ m1k:(rows mkk)
