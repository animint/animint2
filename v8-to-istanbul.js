const fs = require('fs');
const path = require('path');
const v8toIstanbul = require('v8-to-istanbul');

async function convertToIstanbul() {
  try {
    const coverageJsonPath = path.join('tests', 'testthat', 'js-coverage.json');
    const outputIstanbulPath = 'coverage-istanbul.json';

    if (!fs.existsSync(coverageJsonPath)) {
      console.error(`Error: Coverage file not found at ${coverageJsonPath}`);
      process.exit(1);
    }
    const rawCoverage = JSON.parse(fs.readFileSync(coverageJsonPath, 'utf8'));
    const istanbulCoverage = {};
    for (const scriptCoverage of rawCoverage.result) {
      const fullPath = scriptCoverage.url;
      if (!fullPath || !fs.existsSync(fullPath)) {
        console.warn(`Skipping non-existent temp file: ${fullPath}`);
        continue;
      }
      // Optionally skip vendor files if their path contains 'vendor'
      if (fullPath.includes('vendor')) {
          continue;
      }
      try {
        const scriptSource = fs.readFileSync(fullPath, 'utf8');
        const converter = v8toIstanbul(fullPath, 0, {
          source: scriptSource
        });
        await converter.load();
        converter.applyCoverage(scriptCoverage.functions);
        const fileCoverage = converter.toIstanbul();
        Object.assign(istanbulCoverage, fileCoverage);
        console.log(`Processed coverage for temp file: ${path.basename(fullPath)}`);
      } catch (err) {
        console.error(`Error processing ${fullPath}:`, err.message);
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
