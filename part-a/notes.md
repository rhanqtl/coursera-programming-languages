学习语言的三大要素：

- 语法
- 类型检查规则
- 求值规则


类型：

- one-of 比如
```sml
datatype TrafficLightColor = Red | Green | Yellow
```
- each-of 比如元组（实际上是 record 的语法糖） / record
- self-reference 比如
```sml
datatype 'a List = Empty | Cons of 'a * 'a List
```


模式匹配
 
 
- `case ... of ...` 表达式
- 赋值：record unpacking
  - 元组：`val (x, y) = (1, 2);`（或 `val {1 = x, 2 = y} = (1, 2);`）
  - record：`val {first = x, middle = y, last = z} = full_name;`
- 函数定义中的模式匹配
```sml
fun full_name {first_name=x, middle_name=y, last_name=z} = 
    x ^ " " ^ y ^ " " ^ z
```
  - 对于 tuple 来说，在函数定义中使用模式匹配跟直接把 tuple 拆开定义没有分别，例如
  接收一个元组并返回元组中数值之和的函数：
  ```sml
  fun sum_triple (x, y, z) =
      x + y + z
  ```
  接收三个整数并返回三者之和的函数：
  ```sml
  fun sum_triple (x, y, z) =
      x + y + z
  ```
  没有任何差别！
  - 实际上 SML 中每个函数都只接受**一个**参数（形式统一），因此函数可以级联调用！

- 可以嵌套
  ```sml
  fun zip list_pair = 
      case list_pair of
	  ([],    [])    => []
        | (x::xs, y::ys) => (x, y)::zip(xs, ys)
        | _              => raise ListLengthMismatch
```

多态：

```sml
fun partial_name {first_name=x, middle_name=y, last_name=z} = 
    x ^ " " ^ z
```

其中 `y` 没用上，因此编译器推导出来的函数类型是 `fn : {first_name:string, middle_name:'a, last_name:string} -> string`


equality type

`''a` 要求替换的具体类型必须能够进行 `=` 运算

