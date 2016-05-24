export class Alea {

    private s0 : number;
    private s1 : number;
    private s2 : number;
    private c  : number;

    constructor(seed) {
        var m = new Mash();

        this.s0 = m.mash(' ');
        this.s1 = m.mash(' ');
        this.s2 = m.mash(' ');
        this.c = 1;
        this.s0 -= m.mash(seed);
        if (this.s0 < 0) {
            this.s0 += 1;
        }
        this.s1 -= m.mash(seed);
        if (this.s1 < 0) {
            this.s1 += 1;
        }
        this.s2 -= m.mash(seed);
        if (this.s2 < 0) {
            this.s2 += 1;
        }
    }

    random() {
        var t = 2091639 * this.s0 + this.c * 2.3283064365386963e-10;
        this.s0 = this.s1;
        this.s1 = this.s2;
        return this.s2 = t - (this.c = t | 0);
    }

    randomBetween(min, max) {
        return this.random() * (max - min) + min;
    }

    randomIntBetween(min, max) {
        return Math.round(this.randomBetween(min, max));
    }
}


class Mash {

    private n : number;

    constructor() {
        this.n = 0xefc8249d;
    }

    mash(data) {
        var h, i, _i, _ref;
        data = data.toString();
        for (i = _i = 0, _ref = data.length - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
            this.n += data.charCodeAt(i);
            h = 0.02519603282416938 * this.n;
            this.n = h >>> 0;
            h -= this.n;
            h *= this.n;
            this.n = h >>> 0;
            h -= this.n;
            this.n += h * 0x100000000;
        }
        return (this.n >>> 0) * 2.3283064365386963e-10;
    }
}

export class ShuffleBag {

    private alea : Alea;
    private bag;
    private values;


    constructor(alea: Alea) {
        this.alea   = alea;
        this.bag    = [];
        this.values = [];
    }

    addBag(bag: ShuffleBag) {
        this.add(bag.values);
    }

    add(items, frequency = 1) {
        var i, _i, _ref;
        for (i = _i = 0, _ref = frequency - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
            this.values = this.values.concat(items);
        }
    }

    next() {
        if (!(this.values.length > 0)) {
            return void 0;
        }

        if (this.bag.length === 0) {
            this.shuffle();
        }

        return this.bag.shift();
    }

    shuffle() {
        var i, shuffle_index, _i, _ref;
        this.bag[0] = this.values[0];
        for (i = _i = 1, _ref = this.values.length - 1; 1 <= _ref ? _i <= _ref : _i >= _ref; i = 1 <= _ref ? ++_i : --_i) {
            shuffle_index = this.alea.randomIntBetween(0, i);
            this.bag[i] = this.bag[shuffle_index];
            this.bag[shuffle_index] = this.values[i];
        }
    }
}
