Types
-----

All types in Kit can define static fields/methods as well as instance methods.

### Structs and unions

```kit
struct MyStruct {
    public var myField: Int;
}

function main() {
    // pointer auto-dereferencing on field access
    var x: Ptr[MyStruct] = struct MyStruct {
        myField: 1
    };

    printf("%li", x.myField);
}
```

```kit
union MyUnion {
    var intField: Int;
    var floatField: Float;
    var stringField: CString;
}
```

### Enums/algebraic data types

```kit
/**
 * Represents a nullable value.
 */
enum Option[T] {
    Some(value: T);
    None;

    public function unwrap(): T {
        match this {
            Some(value): value;
            default: throw "Attempt to unwrap an empty Option value";
        }
    }
}
```

### Abstracts

```kit
/**
 * Provides convenience methods for working with colors. Variables can be
 * typed as Colors, but at runtime they'll be `unsigned long` with zero
 * overhead.
 */
abstract Color: Uint32 {
    public function getRgb(): (Float, Float, Float) {
        return (this & 0xff0000 >> 16, this & 0xff00 >> 8, this & 0xff);
    }
}

function printRgb(c: Color) {
    print(c.getRgb());
}

function main() {
    printRgb(0xff8080);
}
```

Traits
------

```kit
trait Writer {
    function write(s: String): Void;
}

implement Writer for File {
    function write(s) {
        fwrite(s, 1, s.length, this);
    }
}
```

Traits can also be specialized to provide a default implementation when none is specified.

```kit
trait Map[K, V] {
    function get(key: K): V;
    function set(key: K, value: V): V;
    function exists(key: K): Bool;
}

specialize Map[Bool, V] as BoolMap[V];
specialize Map[Integral, V] as IntMap[V];
specialize Map[String, V] as StringMap[V];

function main() {
    var myMap: Map[Int, String] = new Map();
    myMap[5] = "this just works!";
}
```

C interoperability
------------------

Kit features seamless interoperability with existing C libraries. Kit compiles to C and its type system exposes C types directly. You can call C functions from Kit or Kit functions from C, by explicitly including header files, no additional bindings required.

```kit
include "stdio.h";

function main() {
    printf("%s\n", "Hello from Kit!");
}
```

Anything declared in the header will be type checked, and `#define` macros can also be used by providing type annotations at the usage site.

```kit
include "SDL2/SDL.h";

function helloSDL() {
    SDL_Init(${SDL_INIT_VIDEO: Uint});
    var window: Ptr[SDL_Window] = SDL_CreateWindow(
        "Hello from Kit!",
        ${SDL_WINDOWPOS_UNDEFINED: Uint},
        ${SDL_WINDOWPOS_UNDEFINED: Uint},
        640, 480,
        ${SDL_WINDOW_SHOWN: Uint}
    );
    SDL_Delay(750);
    SDL_DestroyWindow(window);
    SDL_Quit();
}
```

Term rewriting
--------------

Term rewriting rules allow compile-time syntax transformation:

```kit
rules Reduce {
    (pow($x + $y, 2)) => pow($x, 2) + 2 * $x * $y + pow($y, 2);
}

function main() {
    using rules Reduce {
        var a = 3;
        var b = 4;
        var c = pow(a + b, 2);
    }
}
```

Rules can match based on both the AST struture and value type information:

```kit
struct MyStruct {
    var myField: Int;
}

rules StructRules {
    (${s: MyStruct}.property) => $s.myField;
}

function main() {
    var s = struct MyStruct {myField: 5};
    using rules StructRules {
        printf("%li\n", s.property);
    }
}
```

Types can define their own rules, which automatically enter scope for expressions containing the type:

```kit
enum List[T] {
    Cons(head: T, tail: List[T]);
    Empty;

    rules {
        // Replace iteration over lists with an optimized version to avoid
        // creating an unnecessary ListIterator struct.
        (for $x in $this {$e}) => {
            var __rest = this;
            while !__rest.empty {
                var $x = __rest.head;
                $e;
                __rest = __rest.tail;
            }
        }

        (${head: T} :: ${tail: List[T]}) => Cons($head, $tail);

        // Define a custom `++` operator to combine lists
        ($this ++ ${other: Self}) => {
            var newList = Empty;
            for i in this {
                newList = i :: newList;
            }
            for i in other {
                newList = i :: newList;
            }
            return list.reverse();
        }
    }
}
```

Implicits
---------

When implicit values are in scope, they'll be used as arguments in functions automatically. A function will look for matching implicit values for each of its arguments from left to right; it will stop looking as soon as it fails to find an implicit for an argument, so implicit arguments must be contiguous and must be the first arguments of the function.

```kit
function getConfigSection(config: Config, sectionName: CString) {
    return config.get(sectionName);
}

function main() {
    var cfg = defaultConfig();

    using implicit cfg {
        var settings = getConfigSection("settings");
        var controls = getConfigSection("controls");
    }
}
```
