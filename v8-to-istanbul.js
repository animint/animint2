const fs = require('fs');
const path = require('path');
const v8toIstanbul = require('v8-to-istanbul');

async function convertToIstanbul() {
  try {
    // Path configuration
    const staticPath = path.join('tests', 'testthat', 'js-coverage.json');
    const shinyPath = path.join('tests', 'testthat', 'shiny-js-coverage.json');
    const outputIstanbulPath = 'coverage-istanbul.json';
    const baseDir = path.join(__dirname, 'inst', 'htmljs');

    const istanbulCoverage = {};
    // helper: process an array of V8 coverage entries
    const processEntries = async (entries, resolvePathFn, label) => {
      for (const entry of entries) {
        const url = entry.url;
        if (!url) continue;
        // Resolve to a readable file path
        const fullPath = resolvePathFn(url);
        if (!fullPath || !fs.existsSync(fullPath)) {
          console.warn(`[${label}] skipping missing: ${fullPath || url}`);
          continue;
        }
        try {
          const src = fs.readFileSync(fullPath, 'utf8');
          const converter = v8toIstanbul(fullPath, 0, { source: src });
          await converter.load();
          // V8 coverage can be entry.functions or entry.result; prefer .functions
          const v8Functions = entry.functions || entry.result || entry;
          converter.applyCoverage(v8Functions);
          const fileCov = converter.toIstanbul();
          Object.assign(istanbulCoverage, fileCov);
          console.log(`[${label}] processed: ${fullPath}`);
        } catch (err) {
          console.error(`[${label}] error processing ${fullPath}:`, err.message);
        }
      }
    };
    // 1) Static coverage
    if (fs.existsSync(staticPath)) {
      console.log(`Processing static coverage from ${staticPath}`);
      const raw = JSON.parse(fs.readFileSync(staticPath, 'utf8'));
      // Map http://localhost:PORT/animint-htmltest/... → inst/htmljs/...
      const resolveStatic = (url) => {
        const rel = url.replace(/^http:\/\/localhost:\d+\/animint-htmltest\//, '');
        if (!rel || rel.startsWith('vendor/')) return null;
        return path.join(baseDir, rel);
      };
      await processEntries(raw.result || [], resolveStatic, 'static');
    } else {
      console.warn(`Static coverage not found at ${staticPath}`);
    }
    // 2) Shiny coverage
    if (fs.existsSync(shinyPath)) {
      console.log(`Processing Shiny coverage from ${shinyPath}`);
      const shiny = JSON.parse(fs.readFileSync(shinyPath, 'utf8'));
      // Shiny urls are absolute temp file paths; use as-is
      const resolveShiny = (url) => (path.isAbsolute(url) ? url : null);
      // If file is a single object {url, result}, normalize to array
      const shinyEntries = Array.isArray(shiny.result)
        ? shiny.result
        : (shiny.result ? [{ url: shiny.url, functions: shiny.result }] : []);
      await processEntries(shinyEntries, resolveShiny, 'shiny');
    } else {
      console.log(`No Shiny coverage at ${shinyPath} (skipping)`);
    }
    if (Object.keys(istanbulCoverage).length === 0) {
      console.error('No valid coverage data was processed');
      process.exit(1);
    }
    fs.writeFileSync(outputIstanbulPath, JSON.stringify(istanbulCoverage, null, 2));
    console.log(`Successfully converted coverage to ${outputIstanbulPath}`);
  } catch (err) {
    console.error('Error converting coverage:', err.message);
    process.exit(1);
  }
}

convertToIstanbul();