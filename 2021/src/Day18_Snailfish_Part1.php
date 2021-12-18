<?php
/*
--- Day 18: Snailfish ---
You descend into the ocean trench and encounter some snailfish. They say they saw the sleigh keys! They'll even tell you which direction the keys went if you help one of the smaller snailfish with his math homework.

Snailfish numbers aren't like regular numbers. Instead, every snailfish number is a pair - an ordered list of two elements. Each element of the pair can be either a regular number or another pair.

Pairs are written as [x,y], where x and y are the elements within the pair. Here are some example snailfish numbers, one snailfish number per line:

[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
This snailfish homework is about addition. To add two snailfish numbers, form a pair from the left and right parameters of the addition operator. For example, [1,2] + [[3,4],5] becomes [[1,2],[[3,4],5]].

There's only one problem: snailfish numbers must always be reduced, and the process of adding two snailfish numbers can result in snailfish numbers that need to be reduced.

To reduce a snailfish number, you must repeatedly do the first action in this list that applies to the snailfish number:

If any pair is nested inside four pairs, the leftmost such pair explodes.
If any regular number is 10 or greater, the leftmost such regular number splits.
Once no action in the above list applies, the snailfish number is reduced.

During reduction, at most one action applies, after which the process returns to the top of the list of actions. For example, if split produces a pair that meets the explode criteria, that pair explodes before other splits occur.

To explode a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any), and the pair's right value is added to the first regular number to the right of the exploding pair (if any). Exploding pairs will always consist of two regular numbers. Then, the entire exploding pair is replaced with the regular number 0.

Here are some examples of a single explode action:

[[[[[9,8],1],2],3],4] becomes [[[[0,9],2],3],4] (the 9 has no regular number to its left, so it is not added to any regular number).
[7,[6,[5,[4,[3,2]]]]] becomes [7,[6,[5,[7,0]]]] (the 2 has no regular number to its right, and so it is not added to any regular number).
[[6,[5,[4,[3,2]]]],1] becomes [[6,[5,[7,0]]],3].
[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] (the pair [3,2] is unaffected because the pair [7,3] is further to the left; [3,2] would explode on the next action).
[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[7,0]]]].
To split a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded down, while the right element of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.

Here is the process of finding the reduced result of [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]:

after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
Once no reduce actions apply, the snailfish number that remains is the actual result of the addition operation: [[[[0,7],4],[[7,8],[6,0]]],[8,1]].

The homework assignment involves adding up a list of snailfish numbers (your puzzle input). The snailfish numbers are each listed on a separate line. Add the first snailfish number and the second, then add that result and the third, then add that result and the fourth, and so on until all numbers in the list have been used once.

For example, the final sum of this list is [[[[1,1],[2,2]],[3,3]],[4,4]]:

[1,1]
[2,2]
[3,3]
[4,4]
The final sum of this list is [[[[3,0],[5,3]],[4,4]],[5,5]]:

[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
The final sum of this list is [[[[5,0],[7,4]],[5,5]],[6,6]]:

[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
Here's a slightly larger example:

[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
The final sum [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] is found after adding up the above snailfish numbers:

  [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
+ [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]

  [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
+ [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
= [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]

  [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
+ [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
= [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]

  [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
+ [7,[5,[[3,8],[1,4]]]]
= [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]

  [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
+ [[2,[2,2]],[8,[8,1]]]
= [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]

  [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
+ [2,9]
= [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]

  [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
+ [1,[[[9,3],9],[[9,0],[0,7]]]]
= [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]

  [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
+ [[[5,[7,4]],7],1]
= [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]

  [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
+ [[[[4,2],2],6],[8,7]]
= [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
To check whether it's the right answer, the snailfish teacher only checks the magnitude of the final sum. The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element. The magnitude of a regular number is just that number.

For example, the magnitude of [9,1] is 3*9 + 2*1 = 29; the magnitude of [1,9] is 3*1 + 2*9 = 21. Magnitude calculations are recursive: the magnitude of [[9,1],[1,9]] is 3*29 + 2*21 = 129.

Here are a few more magnitude examples:

[[1,2],[[3,4],5]] becomes 143.
[[[[0,7],4],[[7,8],[6,0]]],[8,1]] becomes 1384.
[[[[1,1],[2,2]],[3,3]],[4,4]] becomes 445.
[[[[3,0],[5,3]],[4,4]],[5,5]] becomes 791.
[[[[5,0],[7,4]],[5,5]],[6,6]] becomes 1137.
[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] becomes 3488.
So, given this example homework assignment:

[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
The final sum is:

[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
The magnitude of this final sum is 4140.

Add up all of the snailfish numbers from the homework assignment in the order they appear. What is the magnitude of the final sum?
*/

// run with: `php ./2021/src/Day18_Snailfish_Part1.php`

$filename = "./2021/resources/day18.in";
$handle = fopen($filename, "r");
if (!$handle) {
    die("!!! Unable to open $$filename");
}
$number = NULL;
while (($line = fgets($handle)) !== false) {
    // process the line read.
    //println("Input line is: " . trim($line));
    [$node, $remaining] = parseNode(trim($line), NULL);
    if (strlen($remaining) > 0) die("Expected to read a full string, but this was left: '$remaining'");
    //println("Parsed number is: " . $node->toString());

    if (trim($line) != $node->toString()) {
        die("Wrong parsing! Expected '" . trim($line) . "' got: '" . $node->toString() . "'");
    }

    $node->areParentsConsistent();

    if (!$number) {
        $number = $node;
    } else {
        $number = $number->add($node);
        //println("After adding: " . $number->toString());
        $number->areParentsConsistent();
    }
    $number = $number->reduce();
    //println("After reduction: " . $number->toString());
    $number->areParentsConsistent();
}

println("Number is: " . $number->toString());
println("Magnitude is: " . $number->magnitude());

fclose($handle);

function println($text) {
    print($text . "\n");
}

function isNode($value) {
  if (gettype($value) == "object") {
    return true;
  } else {
    return false;
  }
}

function isValue($value) {
  return !isNode($value);
}

// returns [Value, String], where Value is either a Node or an Int, and String is the remaining to be parsed
function parseNodePart($input, $parent) {
    if ($input[0] == '[') {
        return parseNode($input, $parent);
    } else {
        return [ intval($input[0]), substr($input, 1) ];
    }
}

// returns [Node, String], where String is the remaining to be parsed
function parseNode($input, $parent) {
    //println("Parsing: " . $input);

    $node = new Node();

    $rem = $input;
    // first char needs to be '['
    $open = $rem[0];
    if ($open != "[") die("Expected '[', got '$open'");
    $rem = substr($rem, 1);

    [ $left, $rem ] = parseNodePart($rem, $node);
    $separator = $rem[0];
    if ($separator != ",") die("Expected ',', got '$separator'");
    $rem = substr($rem, 1);
    [ $right, $rem ] = parseNodePart($rem, $node);

    $close = $rem[0];
    if ($close != "]") die("Expected ']', got '$close'");
    $rem = substr($rem, 1);

    $node->parent = $parent;
    $node->left = $left;
    $node->right = $right;

    // println($input . " -> " . $node->toString() . ", rem: " . $rem);

    return [$node, $rem];
}

class Node
{
    public $parent;
    public $left;
    public $right;

    function isRoot() {
        return is_null($this->parent);
    }

    function isLeftNode() {
        if ($this->isRoot()) {
            return false;
        }
        // checking the type too to avoid warnings
        $leftNode = $this->parent->left;
        return $leftNode === $this; // === checks that they are equal and have the same type
    }

    function isRightNode() {
        if ($this->isRoot()) {
            return false;
        }
        // checking the type too to avoid warnings
        $rightNode = $this->parent->right;
        return $rightNode === $this; // === checks that they are equal and have the same type
    }

    public function add($otherNode) {
        $newParent = new Node();
        $newParent->left = $this;
        $newParent->right = $otherNode;

        $this->parent = $newParent;
        $otherNode->parent = $newParent;

        return $newParent;
    }

    public function reduce() {
        //$newRoot = $this;
        $should_continue = true;
        while ($should_continue) {
            $should_continue = false;
            // try exploding, if exploded something -> restart loop and check again
            if ($this->explode()) {
                //println("Exploded into: " . $this->toString());
                $this->areParentsConsistent();
                $should_continue = true;
                continue;
            }

            // try splitting, if split something -> restart loop and check again
            if ($this->split()) {
                //println("Split into: " . $this->toString());
                $this->areParentsConsistent();
                $should_continue = true;
                continue;
            }

            // when nothing got exploded or split, the reduction is finished
        }
        return $this;
    }

    // To split a regular number, replace it with a pair;
    // the left element of the pair should be the regular number divided by two and rounded down,
    // while the right element of the pair should be the regular number divided by two and rounded up.
    // For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.
    public function split() {
        if (isValue($this->left)) {
            if ($this->left > 9) {
                $this->left = splitValue($this, $this->left);
                return true;
            }
        } else {
            if ($this->left->split()) {
                return true;
            }
        }

        if (isValue($this->right)) {
            if ($this->right > 9) {
                $this->right = splitValue($this, $this->right);
                return true;
            }
        } else {
            if ($this->right->split()) {
                return true;
            }
        }

        return false;
    }

    public function explode() {
        return $this->tryExplode(0);
    }

    function tryExplode($depth) {
        if ($depth == 4) {
            // println("Found a depth=4 with: " . $this->toString());
            // Exploding pairs will always consist of two regular numbers.
            if (isNode($this->left) || isNode($this->right)) {
                die("Should have had only literals below depth=4, but got one");
            }

            // println("Node is: " . $this->toString());

            $isLeft = $this->isLeftNode();
            // println("The node is on the " . ($isLeft ? "left" : "right"));

            // the pair's left value is added to the first regular number to the left of the exploding pair (if any),
            $leftNode = $this->findLeftNodeWithRegularNumber();
            if (!is_null($leftNode)) {
                // println("Previous node is: " . $leftNode->toString());
                if (isValue($leftNode->right)) {
                    $leftNode->right = $leftNode->right + $this->left;
                } else {
                    $leftNode->left = $leftNode->left + $this->left;
                }
                // println("Previous node updated to: " . $leftNode->toString());
            } else {
                // println("Previous node is null");
            }

            // and the pair's right value is added to the first regular number to the right of the exploding pair (if any).
            $rightNode = $this->findRightNodeWithRegularNumber();
            if (!is_null($rightNode)) {
                // println("Next node is: " . $rightNode->toString());
                if (isValue($rightNode->left)) {
                    $rightNode->left = $rightNode->left + $this->right;
                } else {
                    $rightNode->right = $rightNode->right + $this->right;
                }
                // println("Next node updated to: " . $rightNode->toString());
            } else {
                // println("Next node is null");
            }

            if ($isLeft) {
                // Then, the entire exploding pair is replaced with the regular number 0.
                $this->parent->left = 0;
            } else {
                // Then, the entire exploding pair is replaced with the regular number 0.
                $this->parent->right = 0;
            }
            return true;
        }
        if (isNode($this->left)) {
            if ($this->left->tryExplode($depth+1)) {
                return true;
            }
        }
        if (isNode($this->right)) {
            if ($this->right->tryExplode($depth+1)) {
                return true;
            }
        }

        return false;
    }

    // find previous node (the one closest to the left, with a literal as the right node/value)
    function findLeftNodeWithRegularNumber() {
        $current = $this;
        while (!$current->isRoot()) {
            //println("looping in to find left node: " . $current->toString());
            if ($current->isLeftNode()) {
                //println("is left node, going to parent: " . );
                $current = $current->parent;
            } else {
                //println("is right node");
                $current = $current->parent;
                if (isValue($current->left)) {
                    return $current;
                }
                $current = $current->left;
                while (isNode($current->right)) {
                    $current = $current->right;
                }
                return $current;
            }
        }
        return NULL;
    }

    // find next node (the one closest to the right, with a literal as the left node/value)
    function findRightNodeWithRegularNumber() {
        $current = $this;
        while (!$current->isRoot()) {
            //println("looping in to find right node: " . $current->toString());
            if ($current->isRightNode()) {
                //println("is right node, going to parent: " . );
                $current = $current->parent;
            } else {
                //println("is left node");
                $current = $current->parent;
                if (isValue($current->right)) {
                    return $current;
                }
                $current = $current->right;
                while (isNode($current->left)) {
                    $current = $current->left;
                }
                return $current;
            }
        }
        return NULL;
    }

    public function areParentsConsistent() {
        $left = $this->left;
        if (isNode($left)) {
            if ($left->parent != $this) {
                die("Parent mismatch: Node: " . $this->toString() . ", left node: " . $left->toString());
            }
            $left->areParentsConsistent();
        }

        $right = $this->right;
        if (isNode($right)) {
            if ($right->parent != $this) {
                die("Parent mismatch: Node: " . $this->toString() . ", right node: " . $right->toString());
            }
            $right->areParentsConsistent();
        }
    }

    public function toString() {
        return "[" . valueToString($this->left) . "," . valueToString($this->right) . "]";
    }

    public function print() {
        print(toString());
    }

    /*
    To check whether it's the right answer, the snailfish teacher only checks the magnitude of the final sum.
    The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element.
    The magnitude of a regular number is just that number.
    */
    public function magnitude() {
        return (3 * magnitude_of($this->left)) + (2 * magnitude_of($this->right));
    }
}

function magnitude_of($node) {
    if (isNode($node)) {
        return $node->magnitude();
    } else {
        return intval($node); // it is an it
    }
}

function splitValue($parent, $value) {
    $newNode = new Node();

    $halfDown = floor($value / 2);

    $newNode->parent = $parent;
    $newNode->left = $halfDown;
    $newNode->right = ($value - $halfDown);
    return $newNode;
}

function valueToString($value) {
    if (isNode($value)) {
        return $value->toString();
    } else {
        return strval($value);
    }
}

?>