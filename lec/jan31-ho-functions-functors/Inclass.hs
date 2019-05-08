module Inclass where

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
b3 = [c3]
b4 = [c2,c1]
b5 = [c3,c2,c1]

sample = [b1,b2,b3,b4,b2,b5,b1,[],b4]

dropEmptyBallots :: [Ballot] -> [Ballot]
dropEmptyBallots = filter (not . null)

getFirstPlaceVotes :: [Ballot] -> [Candidate]
getFirstPlaceVotes = map head . dropEmptyBallots

findLeastVotedFor :: [Candidate] -> Candidate
findLeastVotedFor = undefined

removeCandidate :: Candidate -> [Ballot] -> [Ballot]
removeCandidate c = map (filter (c /=))

runElection :: [Ballot] -> Maybe Candidate
runElection = undefined
