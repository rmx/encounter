var gulp       = require('gulp')
  , ts         = require('gulp-typescript')
  , tslint     = require('gulp-tslint')
  , babel      = require('gulp-babel')
  ;



var shard = ts.createProject({
    declarationFiles: false,
    target: 'ES6',
    typescript: require('typescript')
});



gulp.task('lint', function() {
    // TODO: Include the vendor directory, and tests.
    return gulp.src(['bin/**/*.ts', 'rmx/**/*.ts'])
        .pipe(tslint())
        .pipe(tslint.report('verbose'));
});



gulp.task('compile', function() {
    return gulp.src(['bin/**/*.ts', 'rmx/**/*.ts', 'vendor/**/*.ts', 'lib/**/*.d.ts', 'test/**/*.ts'], { base: './' })
        .pipe(ts(shard))
        .pipe(babel({ modules: "common" }))
        .pipe(gulp.dest('dist/build/'));
});

gulp.task('compile:watch', function() {
    gulp.watch(['bin/**/*.ts', 'rmx/**/*.ts'], ['compile']);
});


gulp.task('default', ['compile', 'compile:watch']);
