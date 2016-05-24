/// <reference path="../Pure/Ids.ts" />
/// <reference path="../data.ts" />

declare var CryptoJS;

module rmx.Core {

    import AccountId = rmx.Pure.AccountId;


    export function
    accountLogin(accountId: AccountId): string {
        return rmx.data.objectContent<rmx.Storage.Account>(<string>accountId).fmap(acc => {
            return acc.login;
        }).get(<string>accountId);
    }


    // TODO: Use a better default avatar image.
    var defaultAvatarUrl = "https://0.gravatar.com/avatar/7f02c799d087a8fa2cc3d4b25bf0d2e3";

    export function
    accountAvatarUrl(accountId: AccountId): string {
        return rmx.data.resolveReferenceString<string>(accountId + ':email').fmap(email => {
            if (email) {
                return [ "//0.gravatar.com/avatar/"
                       , CryptoJS.MD5(email.toLowerCase())
                       , '?d=identicon'
                       ].join('');
            } else {
                throw new Error("Account does not have any email");
            }
        }).get(defaultAvatarUrl);
    }


    export function
    accountFriends(accountId: AccountId): Computation<AccountId[]> {
        return data.findById<rmx.Storage.Account>(<string>accountId).fmap(account => {
            return account.content.friends;
        });
    }
}
