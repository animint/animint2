const fs = require('fs');
const path = require('path');
const v8toIstanbul = require('v8-to-istanbul');

async function convertToIstanbul() {
  try {
    // Path configuration
    const coverageJsonPath = path.join('tests', 'testthat', 'js-coverage.json');
    const outputIstanbulPath = 'coverage-istanbul.json';
    const baseDir = path.join(__dirname, 'inst', 'htmljs');

    console.log(`Reading coverage data from: ${coverageJsonPath}`);
    console.log(`Looking for source files in: ${baseDir}`);

    // Check if input file exists
    if (!fs.existsSync(coverageJsonPath)) {
      console.error(`Error: Coverage file not found at ${coverageJsonPath}`);
      process.exit(1);
    }
    const rawCoverage = JSON.parse(fs.readFileSync(coverageJsonPath, 'utf8'));
    const istanbulCoverage = {};
    // Process each file's coverage
    for (const scriptCoverage of rawCoverage.result) {
      const url = scriptCoverage.url;
      // Skip empty URLs
      if (!url) continue;
      // Extract the relative file path from the URL
      const filePath = url.replace(/^http:\/\/localhost:\d+\/animint-htmltest\//, '');
      if (filePath.startsWith('vendor/')) {
        //Skip files under vendor/
        continue;
      }
      const fullPath = path.join(baseDir, filePath);
      // Skip if path is empty or file doesn't exist
      if (!filePath || !fs.existsSync(fullPath)) {
        console.warn(`File not found or path empty: ${fullPath}`);
        continue;
      }
      try {
        const scriptSource = fs.readFileSync(fullPath, 'utf8');
        // Create converter for this file
        const converter = v8toIstanbul(fullPath, 0, {
          source: scriptSource
        });
        await converter.load();
        converter.applyCoverage(scriptCoverage.functions);
        // Get Istanbul coverage data for this file and merge it
        const fileCoverage = converter.toIstanbul();
        Object.assign(istanbulCoverage, fileCoverage);
        console.log(`Processed coverage for: ${filePath}`);
      } catch (err) {
        console.error(`Error processing ${filePath}:`, err.message);
      }
    }
    // Save Istanbul coverage data
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