class A extends Object {
    Object x;
    A(Object x) {
        super();
        this.x = x;
    }
}

class B extends A {
    B(Object x) {
        super(new Object());
    }
}
