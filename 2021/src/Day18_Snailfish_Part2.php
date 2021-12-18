<?php
/*
--- Part Two ---
You notice a second question on the back of the homework assignment:

What is the largest magnitude you can get from adding only two of the snailfish numbers?

Note that snailfish addition is not commutative - that is, x + y and y + x can produce different results.

Again considering the last example homework assignment above:

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
The largest magnitude of the sum of any two snailfish numbers in this list is 3993. This is the magnitude of [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]] + [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]], which reduces to [[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]].

What is the largest magnitude of any sum of two different snailfish numbers from the homework assignment?
*/

// run with: `php ./2021/src/Day18_Snailfish_Part2.php`

$filename = "./2021/resources/day18.in";
$handle = fopen($filename, "r");
if (!$handle) {
    die("!!! Unable to open $$filename");
}
$numbers = [];
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

    $cloned = clone $node;
    $node->areParentsConsistent();
    $cloned->areParentsConsistent();

    array_push($numbers, $node);
}

$max_magnitude = 0;
for ($i = 0; $i < count($numbers) - 1; $i++) {
    $a = $numbers[$i];
    for ($j = $i+1; $j < count($numbers); $j++) {
        $b = $numbers[$j];

        $max_magnitude = max($max_magnitude, add_and_get_magnitude($a, $b));
        $max_magnitude = max($max_magnitude, add_and_get_magnitude($b, $a));
    }
}

println("Max Magnitude is: $max_magnitude");

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

    function __clone() {
        // make a deep copy and update the parent link
        if (isNode($this->left)) {
            $this->left = clone $this->left;
            $this->left->parent = $this;
        }

        if (isNode($this->right)) {
            $this->right = clone $this->right;
            $this->right->parent = $this;
        }
    }
}

function add_and_get_magnitude($a, $b) {
    $a = clone $a;
    $b = clone $b;
    return $a->add($b)->reduce()->magnitude();
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