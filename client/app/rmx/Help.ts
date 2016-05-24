/// <reference path="./Help/Types.ts" />
/// <reference path="./Help/Articles.ts" />

module rmx.Help {

    export function
    articleUrl(articleId: string): string {
        for (var k in allArticles) {
            var article = allArticles[k];
            if (article.id === articleId) {
                return article.url;
            }
        }
    }
}
