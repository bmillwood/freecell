const { Elm } = require('./check.js');

Elm.Check.init().ports.report.subscribe(function (checks) {
    const failures = checks.filter((check) => !check.ok);
    failures.forEach((check) => console.log('FAIL ' + check.name));
    if (failures.length > 0) {
        console.log(failures.length + ' of ' + checks.length + ' checks failed');
        process.exit(1);
    }
    console.log(checks.length + ' checks passed');
});
