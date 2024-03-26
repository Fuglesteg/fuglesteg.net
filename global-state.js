/*
Local vars to elements
More clear evaluation
and conversion from lisp to js code
*/
const elements = {
  counter1 = {
    val = 1
  },
  counter2 = {
    val = 2
  }
}

/*
Global values no concern for elements
Then we can reuse vars in child comps via props
*/
const values = {
  counter1_val: 1,
  counter2_val: 2
}

const handlers = {
  
}

const handler = {
  set(target, prop, newValue) {
    handlers[prop]?.(newValue);
    Reflect.set(target, prop, newValue);
  }
}

const proxy = new Proxy(values, handler);
