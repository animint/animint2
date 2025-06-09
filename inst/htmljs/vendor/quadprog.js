/**
 * JavaScript implementation of quadratic programming solver
 * Based on the algorithm from quadprog package
 */


function dpofa(a, lda, n) {
    let info, jm1, t, s;

    for (let j = 1; j <= n; j += 1) {
        info = j;
        s = 0;
        jm1 = j - 1;
        if (jm1 < 1) {
            s = a[j][j] - s;
        } else {
            for (let k = 1; k <= jm1; k += 1) {

                // t = a[k][j] - ddot(k - 1, a[1][k], 1, a[1][j], 1);
                t = a[k][j];
                for (let i = 1; i < k; i += 1) {
                    t -= a[i][j] * a[i][k];
                }
                t /= a[k][k];
                a[k][j] = t;
                s += t * t;
            }
            s = a[j][j] - s;
        }

        if (s <= 0) {
            break;
        }

        a[j][j] = Math.sqrt(s);
        info = 0;
    }

    return info;
}


function dpori(a, lda, n) {
    let kp1, t;

    for (let k = 1; k <= n; k += 1) {
        a[k][k] = 1 / a[k][k];
        t = -a[k][k];

        // dscal(k - 1, t, a[1][k], 1);
        for (let i = 1; i < k; i += 1) {
            a[i][k] *= t;
        }

        kp1 = k + 1;
        if (n < kp1) {
            break;
        }
        for (let j = kp1; j <= n; j += 1) {
            t = a[k][j];
            a[k][j] = 0;

            // daxpy(k, t, a[1][k], 1, a[1][j], 1);
            for (let i = 1; i <= k; i += 1) {
                a[i][j] += t * a[i][k];
            }
        }
    }
}


function dposl(a, lda, n, b) {
    let k, t;

    for (k = 1; k <= n; k += 1) {

        // t = ddot(k - 1, a[1][k], 1, b[1], 1);
        t = 0;
        for (let i = 1; i < k; i += 1) {
            t += a[i][k] * b[i];
        }

        b[k] = (b[k] - t) / a[k][k];
    }

    for (let kb = 1; kb <= n; kb += 1) {
        k = n + 1 - kb;
        b[k] /= a[k][k];
        t = -b[k];

        // daxpy(k - 1, t, a[1][k], 1, b[1], 1);
        for (let i = 1; i < k; i += 1) {
            b[i] += t * a[i][k];
        }
    }
}

"use strict";

let epsilon = 1.0e-60;
let tmpa;
let tmpb;
let vsmall = 1.0e-60;

do {
    epsilon += epsilon;
    tmpa = 1 + 0.1 * epsilon;
    tmpb = 1 + 0.2 * epsilon;
} while (tmpa <= 1 || tmpb <= 1);


function qpgen2(dmat, dvec, fddmat, n, sol, lagr, crval, amat, bvec, fdamat, q, meq, iact, nnact, iter, work, ierr) {
    let l1, it1, nvl, nact, temp, sum, t1, tt, gc, gs, nu, t1inf, t2min, go;

    const r = Math.min(n, q);

    let l = 2 * n + (r * (r + 5)) / 2 + 2 * q + 1;

    for (let i = 1; i <= n; i += 1) {
        work[i] = dvec[i];
    }
    for (let i = n + 1; i <= l; i += 1) {
        work[i] = 0;
    }
    for (let i = 1; i <= q; i += 1) {
        iact[i] = 0;
        lagr[i] = 0;
    }

    if (ierr[1] === 0) {
        const info = dpofa(dmat, fddmat, n);

        if (info !== 0) {
            ierr = 2; // eslint-disable-line
            return;
        }
        dposl(dmat, fddmat, n, dvec);
        dpori(dmat, fddmat, n);
    } else {
        for (let j = 1; j <= n; j += 1) {
            sol[j] = 0;
            for (let i = 1; i <= j; i += 1) {
                sol[j] += dmat[i][j] * dvec[i];
            }
        }
        for (let j = 1; j <= n; j += 1) {
            dvec[j] = 0;
            for (let i = j; i <= n; i += 1) {
                dvec[j] += dmat[j][i] * sol[i];
            }
        }
    }

    crval[1] = 0;
    for (let j = 1; j <= n; j += 1) {
        sol[j] = dvec[j];
        crval[1] += work[j] * sol[j];
        work[j] = 0;
        for (let i = j + 1; i <= n; i += 1) {
            dmat[i][j] = 0;
        }
    }
    crval[1] = -crval[1] / 2;
    ierr[1] = 0;

    const iwzv = n;
    const iwrv = iwzv + n;
    const iwuv = iwrv + r;
    const iwrm = iwuv + r + 1;
    const iwsv = iwrm + (r * (r + 1)) / 2;
    const iwnbv = iwsv + q;

    for (let i = 1; i <= q; i += 1) {
        sum = 0;
        for (let j = 1; j <= n; j += 1) {
            sum += amat[j][i] * amat[j][i];
        }
        work[iwnbv + i] = Math.sqrt(sum);
    }

    nact = nnact;

    iter[1] = 0;
    iter[2] = 0;

    function fnGoto50() {
        iter[1] += 1;

        l = iwsv;
        for (let i = 1; i <= q; i += 1) {
            l += 1;
            sum = -bvec[i];
            for (let j = 1; j <= n; j += 1) {
                sum += amat[j][i] * sol[j];
            }
            if (Math.abs(sum) < vsmall) {
                sum = 0;
            }
            if (i > meq) {
                work[l] = sum;
            } else {
                work[l] = -Math.abs(sum);
                if (sum > 0) {
                    for (let j = 1; j <= n; j += 1) {
                        amat[j][i] = -amat[j][i];
                    }
                    bvec[i] = -bvec[i];
                }
            }
        }

        for (let i = 1; i <= nact; i += 1) {
            work[iwsv + iact[i]] = 0;
        }

        nvl = 0;
        temp = 0;
        for (let i = 1; i <= q; i += 1) {
            if (work[iwsv + i] < temp * work[iwnbv + i]) {
                nvl = i;
                temp = work[iwsv + i] / work[iwnbv + i];
            }
        }
        if (nvl === 0) {
            for (let i = 1; i <= nact; i += 1) {
                lagr[iact[i]] = work[iwuv + i];
            }
            return 999;
        }

        return 0;
    }

    function fnGoto55() {
        for (let i = 1; i <= n; i += 1) {
            sum = 0;
            for (let j = 1; j <= n; j += 1) {
                sum += dmat[j][i] * amat[j][nvl];
            }
            work[i] = sum;
        }

        l1 = iwzv;
        for (let i = 1; i <= n; i += 1) {
            work[l1 + i] = 0;
        }
        for (let j = nact + 1; j <= n; j += 1) {
            for (let i = 1; i <= n; i += 1) {
                work[l1 + i] = work[l1 + i] + dmat[i][j] * work[j];
            }
        }

        t1inf = true;
        for (let i = nact; i >= 1; i -= 1) {
            sum = work[i];
            l = iwrm + (i * (i + 3)) / 2;
            l1 = l - i;
            for (let j = i + 1; j <= nact; j += 1) {
                sum -= work[l] * work[iwrv + j];
                l += j;
            }
            sum /= work[l1];
            work[iwrv + i] = sum;
            if (iact[i] <= meq) {
                continue;
            }
            if (sum <= 0) {
                continue;
            }
            t1inf = false;
            it1 = i;
        }

        if (!t1inf) {
            t1 = work[iwuv + it1] / work[iwrv + it1];
            for (let i = 1; i <= nact; i += 1) {
                if (iact[i] <= meq) {
                    continue;
                }
                if (work[iwrv + i] <= 0) {
                    continue;
                }
                temp = work[iwuv + i] / work[iwrv + i];
                if (temp < t1) {
                    t1 = temp;
                    it1 = i;
                }
            }
        }

        sum = 0;
        for (let i = iwzv + 1; i <= iwzv + n; i += 1) {
            sum += work[i] * work[i];
        }
        if (Math.abs(sum) <= vsmall) {
            if (t1inf) {
                ierr[1] = 1;

                return 999; // GOTO 999
            }
            for (let i = 1; i <= nact; i += 1) {
                work[iwuv + i] = work[iwuv + i] - t1 * work[iwrv + i];
            }
            work[iwuv + nact + 1] = work[iwuv + nact + 1] + t1;

            return 700; // GOTO 700
        }
        sum = 0;
        for (let i = 1; i <= n; i += 1) {
            sum += work[iwzv + i] * amat[i][nvl];
        }
        tt = -work[iwsv + nvl] / sum;
        t2min = true;
        if (!t1inf) {
            if (t1 < tt) {
                tt = t1;
                t2min = false;
            }
        }

        for (let i = 1; i <= n; i += 1) {
            sol[i] += tt * work[iwzv + i];
            if (Math.abs(sol[i]) < vsmall) {
                sol[i] = 0;
            }
        }

        crval[1] += tt * sum * (tt / 2 + work[iwuv + nact + 1]);
        for (let i = 1; i <= nact; i += 1) {
            work[iwuv + i] = work[iwuv + i] - tt * work[iwrv + i];
        }
        work[iwuv + nact + 1] = work[iwuv + nact + 1] + tt;

        if (t2min) {
            nact += 1;
            iact[nact] = nvl;

            l = iwrm + ((nact - 1) * nact) / 2 + 1;
            for (let i = 1; i <= nact - 1; i += 1) {
                work[l] = work[i];
                l += 1;
            }

            if (nact === n) {
                work[l] = work[n];
            } else {
                for (let i = n; i >= nact + 1; i -= 1) {
                    if (work[i] === 0) {
                        continue;
                    }
                    gc = Math.max(Math.abs(work[i - 1]), Math.abs(work[i]));
                    gs = Math.min(Math.abs(work[i - 1]), Math.abs(work[i]));
                    if (work[i - 1] >= 0) {
                        temp = Math.abs(gc * Math.sqrt(1 + gs * gs /
                            (gc * gc)));
                    } else {
                        temp = -Math.abs(gc * Math.sqrt(1 + gs * gs /
                            (gc * gc)));
                    }
                    gc = work[i - 1] / temp;
                    gs = work[i] / temp;

                    if (gc === 1) {
                        continue;
                    }
                    if (gc === 0) {
                        work[i - 1] = gs * temp;
                        for (let j = 1; j <= n; j += 1) {
                            temp = dmat[j][i - 1];
                            dmat[j][i - 1] = dmat[j][i];
                            dmat[j][i] = temp;
                        }
                    } else {
                        work[i - 1] = temp;
                        nu = gs / (1 + gc);
                        for (let j = 1; j <= n; j += 1) {
                            temp = gc * dmat[j][i - 1] + gs * dmat[j][i];
                            dmat[j][i] = nu * (dmat[j][i - 1] + temp) -
                                dmat[j][i];
                            dmat[j][i - 1] = temp;

                        }
                    }
                }
                work[l] = work[nact];
            }
        } else {
            sum = -bvec[nvl];
            for (let j = 1; j <= n; j += 1) {
                sum += sol[j] * amat[j][nvl];
            }
            if (nvl > meq) {
                work[iwsv + nvl] = sum;
            } else {
                work[iwsv + nvl] = -Math.abs(sum);
                if (sum > 0) {
                    for (let j = 1; j <= n; j += 1) {
                        amat[j][nvl] = -amat[j][nvl];
                    }
                    bvec[nvl] = -bvec[nvl];
                }
            }

            return 700; // GOTO 700
        }

        return 0;
    }

    function fnGoto797() {
        l = iwrm + (it1 * (it1 + 1)) / 2 + 1;
        l1 = l + it1;
        if (work[l1] === 0) {
            return 798; // GOTO 798
        }
        gc = Math.max(Math.abs(work[l1 - 1]), Math.abs(work[l1]));
        gs = Math.min(Math.abs(work[l1 - 1]), Math.abs(work[l1]));
        if (work[l1 - 1] >= 0) {
            temp = Math.abs(gc * Math.sqrt(1 + (gs / gc) * (gs / gc)));
        } else {
            temp = -Math.abs(gc * Math.sqrt(1 + (gs / gc) * (gs / gc)));
        }
        gc = work[l1 - 1] / temp;
        gs = work[l1] / temp;

        if (gc === 1) {
            return 798; // GOTO 798
        }
        if (gc === 0) {
            for (let i = it1 + 1; i <= nact; i += 1) {
                temp = work[l1 - 1];
                work[l1 - 1] = work[l1];
                work[l1] = temp;
                l1 += i;
            }
            for (let i = 1; i <= n; i += 1) {
                temp = dmat[i][it1];
                dmat[i][it1] = dmat[i][it1 + 1];
                dmat[i][it1 + 1] = temp;
            }
        } else {
            nu = gs / (1 + gc);
            for (let i = it1 + 1; i <= nact; i += 1) {
                temp = gc * work[l1 - 1] + gs * work[l1];
                work[l1] = nu * (work[l1 - 1] + temp) - work[l1];
                work[l1 - 1] = temp;
                l1 += i;
            }
            for (let i = 1; i <= n; i += 1) {
                temp = gc * dmat[i][it1] + gs * dmat[i][it1 + 1];
                dmat[i][it1 + 1] = nu * (dmat[i][it1] + temp) -
                    dmat[i][it1 + 1];
                dmat[i][it1] = temp;
            }
        }

        return 0;
    }

    function fnGoto798() {
        l1 = l - it1;
        for (let i = 1; i <= it1; i += 1) {
            work[l1] = work[l];
            l += 1;
            l1 += 1;
        }

        work[iwuv + it1] = work[iwuv + it1 + 1];
        iact[it1] = iact[it1 + 1];
        it1 += 1;
        if (it1 < nact) {
            return 797; // GOTO 797
        }

        return 0;
    }

    function fnGoto799() {
        work[iwuv + nact] = work[iwuv + nact + 1];
        work[iwuv + nact + 1] = 0;
        iact[nact] = 0;
        nact -= 1;
        iter[2] += 1;

        return 0;
    }

    while (true) {
        go = fnGoto50();
        if (go === 999) {
            return;
        }
        while (true) {
            go = fnGoto55();
            if (go === 0) {
                break;
            }
            if (go === 999) {
                return;
            }
            if (go === 700) {
                if (it1 === nact) {
                    fnGoto799();
                } else {
                    while (true) {
                        fnGoto797();
                        go = fnGoto798();
                        if (go !== 797) {
                            break;
                        }
                    }
                    fnGoto799();
                }
            }
        }
    }

}

function solveQP(Dmat, dvec, Amat, bvec = [], meq = 0, factorized = [0, 0]) {
    const crval = [];
    const iact = [];
    const sol = [];
    const lagr = [];
    const work = [];
    const iter = [];

    let message = "";

    // In Fortran the array index starts from 1
    const n = Dmat.length - 1;
    const q = Amat[1].length - 1;

    if (!bvec) {
        for (let i = 1; i <= q; i += 1) {
            bvec[i] = 0;
        }
    }

    if (n !== Dmat[1].length - 1) {
        message = "Dmat is not symmetric!";
    }
    if (n !== dvec.length - 1) {
        message = "Dmat and dvec are incompatible!";
    }
    if (n !== Amat.length - 1) {
        message = "Amat and dvec are incompatible!";
    }
    if (q !== bvec.length - 1) {
        message = "Amat and bvec are incompatible!";
    }
    if ((meq > q) || (meq < 0)) {
        message = "Value of meq is invalid!";
    }

    if (message !== "") {
        return {
            message
        };
    }

    for (let i = 1; i <= q; i += 1) {
        iact[i] = 0;
        lagr[i] = 0;
    }

    const nact = 0;
    const r = Math.min(n, q);

    for (let i = 1; i <= n; i += 1) {
        sol[i] = 0;
    }
    crval[1] = 0;
    for (let i = 1; i <= (2 * n + (r * (r + 5)) / 2 + 2 * q + 1); i += 1) {
        work[i] = 0;
    }
    for (let i = 1; i <= 2; i += 1) {
        iter[i] = 0;
    }

    qpgen2(Dmat, dvec, n, n, sol, lagr, crval, Amat, bvec, n, q, meq, iact, nact, iter, work, factorized);

    if (factorized[1] === 1) {
        message = "constraints are inconsistent, no solution!";
    }
    if (factorized[1] === 2) {
        message = "matrix D in quadratic function is not positive definite!";
    }

    return {
        solution: sol,
        Lagrangian: lagr,
        value: crval,
        unconstrained_solution: dvec, // eslint-disable-line camelcase
        iterations: iter,
        iact,
        message
    };
}

function optimizeAlignedBoxes(data, alignment, min_distance) {
    console.log("got data : ", data, "alignement : ", alignment)
  var n = data.length;

  if (n === 1) {
    console.log("Only one label, skipping QP optimization.");
    data[0].optimizedPos = alignment === "vertical" ? data[0].scaledY : data[0].scaledX;
    return;
  }
  if (n > 100) {
    console.warn("Too many labels (" + n + "), skipping QP optimization.");
    data.forEach(function(d) {
      d.optimizedPos = alignment === "vertical" ? d.scaledY : d.scaledX;
    });
    return;
  }

var Dmat = [];
var dvec = [];
    for (var i = 0; i < n; i++) {
        Dmat[i] = [];
        for (var j = 0; j < n; j++) Dmat[i][j] = (i == j ? 1 : 0);
        dvec[i] = alignment === "vertical" ? data[i].scaledY : data[i].scaledX;
    }
    var Amat = [];
    var bvec = [];
    for (var i = 0; i < n - 1; i++) {
        for (var j = i + 1; j < n; j++) {
            var constraint = new Array(n).fill(0);
            constraint[i] = 1;
            constraint[j] = -1;
            Amat.push(constraint);
            var min_dist = alignment === "vertical"
            ? (data[i].boxHeight + data[j].boxHeight) / 2 + min_distance
            : (data[i].boxWidth + data[j].boxWidth) / 2 + min_distance;
            bvec.push(min_dist);
        }
    }

    function transpose(matrix) {
    return matrix[0].map((_, colIndex) => matrix.map(row => row[colIndex]));
    }

  try {
    var tAmat = transpose(Amat);
    console.log("QP Inputs:");
    console.log("Dmat:", Dmat);
    console.log("dvec:", dvec);
    console.log("Amat (transposed):", tAmat);
    console.log("bvec:", bvec);

    var qpSolution = solveQP(Dmat, dvec, tAmat, bvec);


    if (!qpSolution || !qpSolution.solution) {
      throw new Error("solveQP did not return a valid solution object.");
    }

    console.log("QP Output:", qpSolution.solution);

    data.forEach(function(d, i) {
      d.optimizedPos = qpSolution.solution[i + 1]; // 1-based index
    });

  } catch (e) {
    console.error("QP solver error:", e);
    console.warn("Falling back to original positions.");
    data.forEach(function(d) {
      d.optimizedPos = alignment === "vertical" ? d.scaledY : d.scaledX;
    });
  }
}
