
type Gen = Int

makeGen :: Int -> Gen
makeGen x = x

doubleGen :: Gen -> (Gen, Int)
doubleGen g = (g+2, g*2)

runGen :: Gen -> [Int]
runGen g = 
        let (newGen, value) = doubleGen g
        in value:runGen newGen

startGen :: Int -> [Int]
startGe n = 
        let newgen = makeGen n
        in  runGen newgen

nGen :: Int -> Int-> [Int]
nGen n genValue = take n $ startGen genValue

