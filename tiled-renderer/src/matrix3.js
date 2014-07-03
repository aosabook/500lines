/**
 * Matrix3 represented by a 3x3 array of floats in column major order.
 */

'use strict'

function Matrix3() {
    this.kRowLength = 3;
    this.kNumItems = this.kRowLength * this.kRowLength;
    this.values = new Float32Array([ 1, 0, 0, 0, 1, 0, 0, 0, 1 ]);
};

/*
 * Requires values to be in column major order.
 */
Matrix3.prototype.set = function(a00, a10, a20, a30,
                                 a01, a11, a21, a31,
                                 a02, a12, a22, a32,
                                 a03, a13, a23, a33) {
    this.values[0] = a00;
    this.values[1] = a10;
    this.values[2] = a20;
    this.values[3] = a30;
    this.values[4] = a01;
    this.values[5] = a11;
    this.values[6] = a21;
    this.values[7] = a31;
    this.values[8] = a02;
};

Matrix3.prototype.clear = function() {
    for (var i = 0; i < this.kNumItems; ++i)
        this.values[i] = 0;
};

Matrix3.prototype.identity = function() {
    this.clear();
    this.setDiagonalValues(1, 1, 1);
};

Matrix3.prototype.translate = function(tx, ty) {
    this.setColumnValues(2,
            this.values[0] * tx + this.values[3] * ty + this.values[6],
            this.values[1] * tx + this.values[4] * ty + this.values[7],
            this.values[2] * tx + this.values[5] * ty + this.values[8]);
};

Matrix3.prototype.scale = function(sx, sy) {
    this.set(this.values[0] * sx, this.values[1] * sx, this.values[2] * sx,
                this.values[3] * sy, this.values[4] * sy, this.values[5] * sy,
                this.values[6], this.values[7], this.values[8]);
};

Matrix3.prototype.setColumnValues = function(column, v0, v1, v2) {
    var i = column * this.kRowLength;
    this.values[i] = v0;
    this.values[i + 1] = v1;
    this.values[i + 2] = v2;
};

Matrix3.prototype.setDiagonalValues = function(v00, v11, v22) {
    this.values[0] = v00;
    this.values[4] = v11;
    this.values[8] = v22;
};
