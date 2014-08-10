type Length = Double
type Weight = Double

data Structure = MobileEnd Double | Mobile Branch Branch
data Branch = Branch Length Structure

makeMobile :: Branch -> Branch -> Structure
makeMobile left right = Mobile left right

makeBranch :: Length -> Structure -> Branch
makeBranch length structure = Branch length structure

leftBranch :: Structure -> Branch
leftBranch (Mobile left _) = left

rightBranch :: Structure -> Branch
rightBranch (Mobile _ right) = right

branchLength :: Branch -> Length
branchLength (Branch length _) = length

branchStructure :: Branch -> Structure
branchStructure (Branch _ structure) = structure

branchWeight :: Branch -> Weight
branchWeight (Branch _ (MobileEnd weight)) = weight
branchWeight (Branch _ (Mobile left right)) = mobileWeight (Mobile left right)

mobileWeight :: Structure -> Weight
mobileWeight (MobileEnd weight) = weight
mobileWeight (Mobile left right) = branchWeight left + branchWeight right

mobileBalanced :: Structure -> Bool
mobileBalanced (Mobile left right) = equalTorque && leftBalanced && rightBalanced
  where equalTorque   = branchTorque left == branchTorque right
        leftBalanced  = branchBalanced left
        rightBalanced = branchBalanced right

branchBalanced :: Branch -> Bool
branchBalanced (Branch _ (MobileEnd _)) = True
branchbalanced (Branch _ mobile) = mobileBalanced mobile

branchTorque :: Branch -> Double
branchTorque (Branch length structure) = length * mobileWeight structure
