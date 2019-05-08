module Inclass where

import Data.List
import qualified Data.HashMap.Strict as S

type AllType a = (a -> Bool) -> [a] -> Bool

--myAll :: AllType a
myAll p = and . map p

--myAll2 :: AllType a
myAll2 p [] = True
myAll2 p (x : xs) = p x && myAll2 p xs

-----------------------------------------
-- preference voting (STV)
-----------------------------------------

type Candidate = String
type Ballot = [Candidate]

c1 = "Driftwood"
c2 = "Skaggs"
c3 = "Boggus"

b1 = [c1,c2]
b2 = [c1]
b3 = [c3,c2]
b4 = [c2,c3]
b5 = [c3,c2,c1]

sample = [b1,b2,b3,b4,b5]

dropEmptyBallots :: [Ballot] -> [Ballot]
dropEmptyBallots = filter (not . null) 

getFirstPlaceVotes :: [Ballot] -> [Candidate]
getFirstPlaceVotes = map head . dropEmptyBallots

type VoteTable = S.HashMap String Int

newVote :: Maybe Int -> Int
newVote Nothing = 1
newVote (Just v) = v+1

addVoteToVoteTable :: Candidate -> VoteTable -> VoteTable
addVoteToVoteTable c vt = S.insert c (newVote (S.lookup c vt)) vt

buildVoteTableH :: [Candidate] -> VoteTable -> VoteTable
buildVoteTableH [] vt = vt
buildVoteTableH (c : cs) vt = buildVoteTableH cs (addVoteToVoteTable c vt)

buildVoteTable :: [Candidate] -> VoteTable 
buildVoteTable cs = buildVoteTableH cs S.empty

compareVotes :: (Candidate,Int) -> (Candidate,Int) -> Ordering
compareVotes (_,v1) (_,v2) = compare v1 v2

getLeastSupported :: VoteTable -> Candidate
getLeastSupported vt = fst (head (sortBy compareVotes (S.toList vt)))

findLeastVotedFor :: [Candidate] -> Candidate
findLeastVotedFor = getLeastSupported . buildVoteTable

removeCandidate :: Candidate -> [Ballot] -> [Ballot]
removeCandidate c = map (filter (c /=))

runElectionH :: Maybe Candidate -> [Ballot] -> Maybe Candidate
runElectionH prev [] = prev
runElectionH prev bs = case getFirstPlaceVotes bs of
                         [] -> prev
                         x -> let c = findLeastVotedFor x in
                                runElectionH (Just c) (removeCandidate c bs)

runElection :: [Ballot] -> Maybe Candidate
runElection = runElectionH Nothing 

