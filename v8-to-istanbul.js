const fs = require('fs');
const path = require('path');
const v8toIstanbul = require('v8-to-istanbul');

function mergeHits(oldHits, newHits) {
        var merged = Object.assign({}, oldHits);
        var key, oldVal, newVal;
        for (key in newHits) {
                oldVal = merged[key];
                newVal = newHits[key];
                if (Array.isArray(newVal)) {
                        merged[key] = newVal.map(function(count, i) {
                                return Math.max(count, Array.isArray(oldVal) ? oldVal[i] || 0 : 0);
                        });
                } else {
                        merged[key] = Math.max(oldVal || 0, newVal);
                }
        }
        return merged;
}

function mergeFileCoverage(existing, incoming) {
        var pathKey, next, prev;
        for (pathKey in incoming) {
                next = incoming[pathKey];
                prev = existing[pathKey];
                if (prev) {
                        prev.s = mergeHits(prev.s, next.s);
                        prev.f = mergeHits(prev.f, next.f);
                        prev.b = mergeHits(prev.b, next.b);
                } else {
                        existing[pathKey] = next;
                }
        }
}

async function convertToIstanbul() {
        try {
                const coverageJsonPath = path.join('tests', 'testthat', 'js-coverage.json');
                const outputIstanbulPath = 'coverage-istanbul.json';
                const baseDir = path.join(__dirname, 'inst', 'htmljs');
                console.log(`Reading coverage data from: ${coverageJsonPath}`);
                console.log(`Looking for source files in: ${baseDir}`);
                if (!fs.existsSync(coverageJsonPath)) {
                        console.error(`Error: Coverage file not found at ${coverageJsonPath}`);
                        process.exit(1);
                }
                const rawCoverage = JSON.parse(fs.readFileSync(coverageJsonPath, 'utf8'));
                const istanbulCoverage = {};
                for (const scriptCoverage of rawCoverage.result) {
                        const url = scriptCoverage.url;
                        if (!url) continue;
                        const filePath = url.replace(/^http:\/\/localhost:\d+\/animint-htmltest\//, '');
                        if (filePath.startsWith('vendor/')) continue;
                        const fullPath = path.join(baseDir, filePath);
                        if (!filePath || !fs.existsSync(fullPath)) continue;
                        try {
                                const converter = v8toIstanbul(fullPath, 0, {
                                        source: fs.readFileSync(fullPath, 'utf8')
                                });
                                await converter.load();
                                converter.applyCoverage(scriptCoverage.functions);
                                mergeFileCoverage(istanbulCoverage, converter.toIstanbul());
                                console.log(`Processed coverage for: ${filePath}`);
                        } catch (err) {
                                console.error(`Error processing ${filePath}:`, err.message);
                        }
                }
                if (Object.keys(istanbulCoverage).length > 0) {
                        fs.writeFileSync(outputIstanbulPath, JSON.stringify(istanbulCoverage, null, 2));
                        console.log(`Successfully converted coverage to ${outputIstanbulPath}`);
                } else {
                        console.error('No valid coverage data was processed');
                        process.exit(1);
                }
        } catch (err) {
                console.error('Error converting coverage:', err.message);
                process.exit(1);
        }
}

convertToIstanbul();
