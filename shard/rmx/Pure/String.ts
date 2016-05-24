// String manipulation fuctions be here.

export function
toString(obj, klass: string, ...propertyNames: string[]): string {
    var props = propertyNames.map(prop);
    return '<' + klass + ' ' + props.join(' ') + '>';

    function prop(name: string) {
        return name + '=' + obj[name];
    }
}

export function
dasherize(x: string): string {
    return x
      .trim()
      .replace(/([A-Z])/g, '-$1')
      .replace(/[-_\s]+/g, '-')
      .toLowerCase();
}

export function
titleize(x: string): string {
    return x
      .toLowerCase()
      .replace(/(?:^|\s|-)\S/g, function(c) { return c.toUpperCase(); });
}

export function
camelize(x: string): string {
    return x.replace(/-+(.)?/g, function(match, chr) {
        // FIXME: unused variable warning;
        (() => { return match; })();

        return chr ? chr.toUpperCase() : '';
    });
}

export function
capitalize(x: string): string {
    return x.charAt(0).toUpperCase() + x.substring(1).toLowerCase();
}

export function
classify(x: string): string {
    return titleize(x.replace(/[\W_]/g, ' ')).replace(/\s/g, '');
}

// Concat a term list into a single string, using correct punctuation
// between the terms. Adds oxford comma to avoid ambiguity.
export function
punctuatedTermList(terms: string[]): string {
    var length = terms.length;

    if (length <= 2) {
        return terms.join(' and ');

    } else {
        var lastIndex = terms.length - 1;

        return terms.map(function(term, index) {
            if (index === lastIndex) {
                return 'and ' + term;
            } else {
                return term;
            }
        }).join(', ');
    }
}
