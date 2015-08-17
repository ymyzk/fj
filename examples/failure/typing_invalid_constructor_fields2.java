class A extends Object {
    A x;
    Object y;
    A(A x, Object y) {
        super();
        this.x = x;
        this.y = y;
    }
}

class B extends Object {
    B b;
    B(A b) {
        super();
        this.b = b;
    }
}

