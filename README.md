# BeAFix

Our tool extends [AlloyAnalyzer 5.1.0](https://github.com/AlloyTools/org.alloytools.alloy),  and  tries to repair an Alloy model marked with expressions to be mutated. It supports repairing expressions inside **_facts_**, **_assertions_**, **_functions_**, and **_predicates_**. A marked expression has the form `{#m#([vars]) expression}` where `vars` is a comma separated list of variables related to the marked expression, this is used by the variabilization pruning technique.

As an example we can consider the following marked expressions:<br/>

`{#m#() some x : T | P[x]}` where the whole quantifier expression is marked for mutation.<br/>
`some x : T | {#m#(x) P[x]}` where only a part of an expression is marked for mutation.<br/>

Our technique is based on exhaustive search of repair candidates (generated by applying mutation operators) up to a certain bound (amount of mutations per marked expressions and/or a specific amount of time). As a way to reduce how many candidates we need to visit in our search, we use several pruning techniques as well as prioritizing partially repaired candidates.

## Builing and Importing to an IDE

Please refer to the [AlloyAnalyzer](https://github.com/AlloyTools/org.alloytools.alloy) project for these instructions.

You might need to manually install gradle and run `gradle wrapper` if gradle wrapper class can't be found.

## Mutation Operators

* **AORB**       : Replaces binary arithmetic operators (**_division_**, **_multiplication_**, **_modulo_**, **_addition_** and **_subtraction_**).
* **BES** 		: Given a binary expression returns both operands as mutations (`a op b` -> {`a`, `b`}).
* **BESOR** 	: Replaces binary set operators (**_join_**, **_union_**, **_diff_**, **_intersection_**, and **_overriding_**).
* **COR** 		: Replaces binary conditional operators (**_and_**, **_or_**, **_implies_**, and **_if and only if_**).
* **CUOI** 		: Negates a boolean expression.
* **EMOR** 	     : Replaces equality operators by membership operators:<br/>
                    `==` <-> `in`<br/>
                    `!=` <-> `!in`<br/>
* **JEE** 		: Extends a join expression, given `a.b` it can generate expressions like `a.b.c` and `a.x.y`.
* **JER** 		: Replaces part of a join expression, given `a.b` it can generate expressions like `a.x` **but will not generate** expressions like `x.y` (which replaces more than one join operand).
* **JES** 		: Reduces a join expression by either removing one join operand or replacing a join expression with another expression with no joins (**_variables_**, **_signatures_**, **_fields_**).
* **MOR** 		: Replaces multiplicity operators in a unary expression (**_no_**, **_some_**, **_lone_**, **_one_**).
* **NESE** 		: Given an expression `A`, this operator will add, for each variable or join `x`, `some x => A`. For example:<br/>
               `list'.rest = list and list'.element = e` will be mutated to\
               `(some list'.rest && some list && some list'.element && some e) => list'.rest = list and list'.element = e`<br/>
* **QTOI** 		: Given an expression `x`, this operators generates `no x`, `some x`, `lone x`, and `one x`.
* **QTOR** 		: Replaces the operator in a quantifier expression (**_all_**, **_lone_**, **_no_**, **_one_**, and **_some_**), for the moment **_comprehension_** and **_sum_** operators are not considered.
* **ROR**        : Replaces relational binary operators (**_equal_**, **_not equal_**, **_greater_**, **_not greater_**, **_greater or equal_**, **_not greater or equal_**, **_less_**, **_not less_**, **_less or equal_**, and **_not less or equal_**).
* **RUOD** 	     : Deletes unary relational operators (**_transpose_**, **_closure_**, and **_reflexive closure_**).
* **RUOI** 		: Inserts unary relational operators (**_transpose_**, **_closure_**, and **_reflexive closure_**).
* **RUOR** 	     : Replaces unary relational operators (**_transpose_**, **_closure_**, and **_reflexive closure_**).
* **SSE** 		: Extends an expression that can be viewed as a set, for example: `x` can be mutated to `x + y`.
* **SSS** 		: Reduces an expression that can be viewed as a set, for example: `x` can be mutated to `x - y`.
* **VCR** 		: Replaces a **_field_**, **_signature_**, **_variable_**, or **_constant_**, which is not a part of a join expression,  with reachable **_variables_**, **_signatures_**, **_fields_**, and **_constants_**.

*All mutations are both type checked (although some invalid mutations can happen); irrelevant mutations are also analysed and skipped, as for example, adding a closure operator to an expression with a reflexive closure operator;  and finally, repeated expressions are also detected and removed from analysis.*

## Adding Mutation Operators

**BeAFix** core is implemented in `org.alloytools.alloy.core.are.edu.unrc.dc`. Inside the `mutation.op` subpackage are all operators. Any new implemented operator must then be added to `Ops` enum, overriding the necesessary methods:
 * `isImplemented() : boolean` must return either `true` or `false` and will determine if that operator will be used.
 * `getOperator(CompModule) : Mutator` must return the implemented operator inside the `mutation.op` subpackage.
 * `getComplexity() : Int` must return a positive number that will determine in which order (w.r.t. other operators) the operator will be used. 
