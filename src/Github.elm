module Github exposing
    ( OAuthToken, oauthToken, oauthTokenToString, AccessTokenResponse, oauthLink, OAuthCode, oauthCode, oauthCodeToString, ClientId, clientId, clientIdToString, ClientSecret, clientSecret, clientSecretToString, getAccessToken, Scope(..), scopeFromString, scopeToString
    , getRepository, getContents, Owner, owner, ownerToString, updateFileContents
    , Branch, branch, branchToString, getBranches, getBranch, updateBranch, listTags, createBranch, getBranchZip, getTag, getCommit, createCommit, getCommitZip, sha, shaToString, ShaHash, CommitSha, TreeSha, Content(..), ContentType(..), DirectoryEntry, createTree
    , PullRequest, getPullRequests, getPullRequest, createPullRequest, createFork
    , getComments, createComment, createIssue
    )

{-|


## Authorization

@docs OAuthToken, oauthToken, oauthTokenToString, AccessTokenResponse, oauthLink, OAuthCode, oauthCode, oauthCodeToString, ClientId, clientId, clientIdToString, ClientSecret, clientSecret, clientSecretToString, getAccessToken, Scope, scopeFromString, scopeToString


## Get repository

@docs getRepository, getContents, Owner, owner, ownerToString, updateFileContents


## Work with git

@docs Branch, branch, branchToString, getBranches, getBranch, updateBranch, listTags, createBranch, getBranchZip, getTag, getCommit, createCommit, getCommitZip, sha, shaToString, ShaHash, CommitSha, TreeSha, Content, ContentType, DirectoryEntry, createTree


## Pull request

@docs PullRequest, getPullRequests, getPullRequest, createPullRequest, createFork


## Issues

@docs getComments, createComment, createIssue

-}

import Base64
import Bytes exposing (Bytes)
import Dict
import Http
import Iso8601
import Json.Decode
import Json.Encode
import List.Nonempty exposing (Nonempty)
import String.Nonempty exposing (NonemptyString)
import Task exposing (Task)
import Time
import Url exposing (Url)
import Url.Builder


{-| See <https://docs.github.com/en/rest/reference/repos#get-a-repository>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getRepository :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    }
    -> Task Http.Error { defaultBranch : Branch }
getRepository params =
    let
        decoder =
            Json.Decode.map (\defaultBranch -> { defaultBranch = defaultBranch })
                (Json.Decode.field "default_branch" decodeBranch)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| Get all branches for a git repo.
-}
getBranches :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    }
    -> Task Http.Error (List { name : String, sha : ShaHash CommitSha })
getBranches params =
    let
        decoder =
            Json.Decode.map2 (\name sha_ -> { name = name, sha = sha_ })
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.at [ "commit", "sha" ] decodeSha)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url =
            Url.Builder.crossOrigin githubApiDomain
                [ "repos", ownerToString params.owner, params.repo, "branches" ]
                []
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decoder)
        , timeout = Nothing
        }


{-| authToken is Maybe here because it seems like there can be problems request a zip from a public repo if you provide authentication.
-}
getBranchZip :
    { authToken : Maybe OAuthToken
    , owner : Owner
    , repo : String
    , branchName : Maybe Branch
    }
    -> Task Http.Error Bytes
getBranchZip params =
    Http.task
        { method = "GET"
        , headers =
            case params.authToken of
                Just authToken ->
                    [ authorizationHeader authToken ]

                Nothing ->
                    []
        , url =
            Url.Builder.crossOrigin githubApiDomain
                ("repos"
                    :: ownerToString params.owner
                    :: params.repo
                    :: "zipball"
                    :: (case params.branchName of
                            Just branchName ->
                                [ branchToString branchName ]

                            Nothing ->
                                []
                       )
                )
                []
        , body = Http.emptyBody
        , resolver = bytesResolver
        , timeout = Nothing
        }


{-| -}
getCommitZip : { authToken : OAuthToken, owner : Owner, repo : String, sha : ShaHash CommitSha } -> Task Http.Error Bytes
getCommitZip params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://github.com/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/archive/" ++ shaToString params.sha ++ ".zip"
        , body = Http.emptyBody
        , resolver = bytesResolver
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getCommit :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , sha : ShaHash CommitSha
    }
    -> Task Http.Error (ShaHash TreeSha)
getCommit params =
    let
        decoder =
            Json.Decode.at [ "tree", "sha" ] decodeSha
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/commits/" ++ shaToString params.sha
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#create-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createCommit :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , message : String
    , tree : ShaHash TreeSha
    , parents : List (ShaHash CommitSha)
    }
    -> Task Http.Error (ShaHash CommitSha)
createCommit params =
    let
        decoder =
            Json.Decode.field "sha" Json.Decode.string
                |> Json.Decode.map sha
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/commits"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "tree", encodeSha params.tree )
                    , ( "parents", Json.Encode.list encodeSha params.parents )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getBranch :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , branchName : Branch
    }
    -> Task Http.Error (ShaHash CommitSha)
getBranch params =
    let
        decoder =
            Json.Decode.at [ "object", "sha" ] decodeSha
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/refs/heads/" ++ branchToString params.branchName
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#update-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
updateBranch :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , branchName : Branch
    , sha : ShaHash CommitSha
    , force : Bool
    }
    -> Task Http.Error (ShaHash CommitSha)
updateBranch params =
    Http.task
        { method = "PATCH"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/refs/heads/" ++ branchToString params.branchName
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "sha", Json.Encode.string (shaToString params.sha) ), ( "force", Json.Encode.bool params.force ) ]
                )
        , resolver = jsonResolver referenceDecoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getTag :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , tagName : String
    }
    -> Task Http.Error (ShaHash CommitSha)
getTag params =
    let
        decoder =
            Json.Decode.at [ "object", "sha" ] decodeSha
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/refs/tags/" ++ params.tagName
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#create-a-tree>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createTree :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , treeNodes : Nonempty { path : String, content : String }
    , baseTree : Maybe (ShaHash TreeSha)
    }
    -> Task Http.Error { treeSha : ShaHash TreeSha }
createTree params =
    let
        decoder =
            Json.Decode.field "sha" decodeSha
                |> Json.Decode.map (\treeSha -> { treeSha = treeSha })
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/trees"
        , body =
            ( "tree", Json.Encode.list encodeTreeNode (List.Nonempty.toList params.treeNodes) )
                :: (case params.baseTree of
                        Just baseTree ->
                            [ ( "base_tree", encodeSha baseTree ) ]

                        Nothing ->
                            []
                   )
                |> Json.Encode.object
                |> Http.jsonBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


encodeTreeNode : { path : String, content : String } -> Json.Encode.Value
encodeTreeNode treeNode =
    ( "path", Json.Encode.string treeNode.path )
        :: ( "mode", Json.Encode.string "100644" )
        :: ( "type", Json.Encode.string "blob" )
        :: ( "content", Json.Encode.string treeNode.content )
        :: []
        |> Json.Encode.object


referenceDecoder =
    Json.Decode.at [ "object", "sha" ] Json.Decode.string
        |> Json.Decode.map sha


type alias Tag =
    { name : String
    , commitSha : ShaHash CommitSha
    , nodeId : String
    }


decodeTag : Json.Decode.Decoder Tag
decodeTag =
    Json.Decode.map3 Tag
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.at [ "commit", "sha" ] decodeSha)
        (Json.Decode.field "node_id" Json.Decode.string)


{-| See <https://docs.github.com/en/rest/reference/repos#list-repository-tags>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
listTags :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    }
    -> Task Http.Error (List Tag)
listTags params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/tags"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decodeTag)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/git/refs/#create-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createBranch :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , branchName : Branch
    , sha : ShaHash CommitSha
    }
    -> Task Http.Error ()
createBranch params =
    let
        decoder =
            Json.Decode.succeed ()
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url =
            Url.Builder.crossOrigin
                githubApiDomain
                [ "repos", ownerToString params.owner, params.repo, "git", "refs" ]
                []
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "ref", Json.Encode.string ("refs/heads/" ++ branchToString params.branchName) )
                    , ( "sha", encodeSha params.sha )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| The data returned by [`getPullRequests`](#getPullRequests).
-}
type alias PullRequest =
    { number : Int
    , title : String
    }


decodePullRequest =
    Json.Decode.map2
        PullRequest
        (Json.Decode.at [ "number" ] Json.Decode.int)
        (Json.Decode.at [ "title" ] Json.Decode.string)


{-| See <https://developer.github.com/v3/pulls/#list-pull-requests>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getPullRequests :
    { authToken : OAuthToken
    , repo : String
    }
    -> Task Http.Error (List PullRequest)
getPullRequests params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decodePullRequest)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getPullRequest :
    { authToken : OAuthToken
    , repo : String
    , number : Int
    }
    ->
        Task
            Http.Error
            { head : { ref : String, sha : ShaHash CommitSha } }
getPullRequest params =
    let
        decoder =
            Json.Decode.map2
                (\headRef headSha ->
                    { head =
                        { ref = headRef
                        , sha = sha headSha
                        }
                    }
                )
                (Json.Decode.at [ "head", "ref" ] Json.Decode.string)
                (Json.Decode.at [ "head", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls/" ++ String.fromInt params.number
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#create-a-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createPullRequest :
    { authToken : OAuthToken
    , destinationOwner : Owner
    , destinationRepo : String
    , destinationBranch : Branch
    , sourceBranchOwner : Owner
    , sourceBranch : Branch
    , title : String
    , description : String
    }
    -> Task Http.Error { apiUrl : String, htmlUrl : String }
createPullRequest params =
    let
        decoder =
            Json.Decode.map2 (\url htmlUrl -> { apiUrl = url, htmlUrl = htmlUrl })
                (Json.Decode.field "url" Json.Decode.string)
                (Json.Decode.field "html_url" Json.Decode.string)
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.destinationOwner ++ "/" ++ params.destinationRepo ++ "/pulls"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "title", Json.Encode.string params.title )
                    , ( "base", encodeBranch params.destinationBranch )
                    , ( "head"
                      , if params.destinationOwner == params.sourceBranchOwner then
                            encodeBranch params.sourceBranch

                        else
                            ownerToString params.sourceBranchOwner
                                ++ ":"
                                ++ branchToString params.sourceBranch
                                |> Json.Encode.string
                      )
                    , ( "body", Json.Encode.string params.description )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| -}
type ContentType
    = FileContentType
    | DirectoryContentType
    | SymLinkType
    | SubmoduleType


{-| -}
type Content
    = FileContent
        { encoding : String
        , content : String
        , sha : ShaHash CommitSha
        , downloadUrl : Url
        , url : Url
        }
    | DirectoryContent (List DirectoryEntry)
    | Symlink
    | Submodule


{-| A file directory in a git repo.
-}
type alias DirectoryEntry =
    { contentType : ContentType
    , name : String
    , path : String
    , sha : ShaHash CommitSha
    , downloadUrl : Maybe Url
    , url : Maybe Url
    }


decodeFile =
    Json.Decode.map5
        (\encoding content sha_ downloadUrl url ->
            { encoding = encoding
            , content = content
            , sha = sha_
            , downloadUrl = downloadUrl
            , url = url
            }
        )
        (Json.Decode.field "encoding" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
        (Json.Decode.field "sha" decodeSha)
        (Json.Decode.field "download_url" decodeUrl)
        (Json.Decode.field "url" decodeUrl)


decodeContentType : Json.Decode.Decoder ContentType
decodeContentType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\text ->
                case text of
                    "file" ->
                        Json.Decode.succeed FileContentType

                    "dir" ->
                        Json.Decode.succeed DirectoryContentType

                    "symlink" ->
                        Json.Decode.succeed SymLinkType

                    "submodule" ->
                        Json.Decode.succeed SubmoduleType

                    _ ->
                        Json.Decode.fail ("Invalid content type: " ++ text)
            )


decodeUrl : Json.Decode.Decoder Url
decodeUrl =
    Json.Decode.string
        |> Json.Decode.andThen
            (\text ->
                case Url.fromString text of
                    Just url ->
                        Json.Decode.succeed url

                    Nothing ->
                        Json.Decode.fail ("Invalid url: " ++ text)
            )


decodeDirectoryEntry : Json.Decode.Decoder DirectoryEntry
decodeDirectoryEntry =
    Json.Decode.map6 DirectoryEntry
        (Json.Decode.field "type" decodeContentType)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "sha" decodeSha)
        (Json.Decode.field "download_url" (Json.Decode.nullable decodeUrl))
        (Json.Decode.field "url" (Json.Decode.nullable decodeUrl))


{-| See <https://docs.github.com/en/rest/reference/repos#contents>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getContents :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    , ref : Maybe String
    , path : String
    }
    -> Task Http.Error Content
getContents params =
    let
        decoder =
            Json.Decode.oneOf
                [ Json.Decode.field "type" decodeContentType
                    |> Json.Decode.andThen
                        (\contentType ->
                            case contentType of
                                FileContentType ->
                                    Json.Decode.map FileContent decodeFile

                                DirectoryContentType ->
                                    Json.Decode.map (List.singleton >> DirectoryContent) decodeDirectoryEntry

                                SymLinkType ->
                                    Json.Decode.succeed Symlink

                                SubmoduleType ->
                                    Json.Decode.succeed Submodule
                        )
                , Json.Decode.list decodeDirectoryEntry |> Json.Decode.map DirectoryContent
                ]
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url =
            Url.Builder.crossOrigin githubApiDomain
                [ "repos"
                , ownerToString params.owner
                , params.repo
                , "contents"
                , params.path
                ]
                (case params.ref of
                    Just ref ->
                        [ Url.Builder.string "ref" ref ]

                    Nothing ->
                        []
                )

        --"https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path ++ "?ref=" ++ params.ref
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/repos/contents/#update-a-file>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
updateFileContents :
    { authToken : OAuthToken
    , repo : String
    , branch : Branch
    , path : String
    , sha : ShaHash a
    , message : String
    , content : String
    }
    ->
        Task
            Http.Error
            { content :
                { sha : ShaHash a
                }
            }
updateFileContents params =
    let
        decoder =
            Json.Decode.map
                (\contentSha ->
                    { content = { sha = contentSha } }
                )
                (Json.Decode.at [ "content", "sha" ] decodeSha)
    in
    Http.task
        { method = "PUT"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "content", Json.Encode.string (Base64.encode params.content) )
                    , ( "sha", encodeSha params.sha )
                    , ( "branch", encodeBranch params.branch )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getComments :
    { authToken : OAuthToken
    , repo : String
    , issueNumber : Int
    }
    ->
        Task
            Http.Error
            (List
                { body : String
                , user :
                    { login : String
                    , avatarUrl : String
                    }
                , createdAt : Time.Posix
                , updatedAt : Time.Posix
                }
            )
getComments params =
    let
        decoder =
            Json.Decode.map5
                (\body userLogin userAvatarUrl createdAt updatedAt ->
                    { body = body
                    , user =
                        { login = userLogin
                        , avatarUrl = userAvatarUrl
                        }
                    , createdAt = createdAt
                    , updatedAt = updatedAt
                    }
                )
                (Json.Decode.at [ "body" ] Json.Decode.string)
                (Json.Decode.at [ "user", "login" ] Json.Decode.string)
                (Json.Decode.at [ "user", "avatar_url" ] Json.Decode.string)
                (Json.Decode.at [ "created_at" ] Iso8601.decoder)
                (Json.Decode.at [ "updated_at" ] Iso8601.decoder)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/issues/" ++ String.fromInt params.issueNumber ++ "/comments"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decoder)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#create-a-comment>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createComment :
    { authToken : OAuthToken
    , repo : String
    , issueNumber : Int
    , body : String
    }
    ->
        Task
            Http.Error
            { body : String
            , user :
                { login : String
                , avatarUrl : String
                }
            , createdAt : Time.Posix
            , updatedAt : Time.Posix
            }
createComment params =
    let
        decoder =
            Json.Decode.map5
                (\body userLogin userAvatarUrl createdAt updatedAt ->
                    { body = body
                    , user =
                        { login = userLogin
                        , avatarUrl = userAvatarUrl
                        }
                    , createdAt = createdAt
                    , updatedAt = updatedAt
                    }
                )
                (Json.Decode.at [ "body" ] Json.Decode.string)
                (Json.Decode.at [ "user", "login" ] Json.Decode.string)
                (Json.Decode.at [ "user", "avatar_url" ] Json.Decode.string)
                (Json.Decode.at [ "created_at" ] Iso8601.decoder)
                (Json.Decode.at [ "updated_at" ] Iso8601.decoder)
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/issues/" ++ String.fromInt params.issueNumber ++ "/comments"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "body", Json.Encode.string params.body )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| The the name of owner of a repo, ("avh4" for example)
-}
type Owner
    = Owner String


{-| -}
owner : String -> Owner
owner =
    Owner


{-| -}
ownerToString : Owner -> String
ownerToString (Owner owner_) =
    owner_


{-| The name of a git branch, i.e. "master", "main", "feature-branch"
-}
type Branch
    = Branch String


{-| -}
branch : String -> Branch
branch =
    Branch


{-| -}
branchToString : Branch -> String
branchToString (Branch branch_) =
    branch_


encodeBranch : Branch -> Json.Encode.Value
encodeBranch =
    branchToString >> Json.Encode.string


decodeBranch : Json.Decode.Decoder Branch
decodeBranch =
    Json.Decode.map branch Json.Decode.string


{-| An OAuth token used to authenticate various Github API requests. Not to be confused with `OAuthCode` which is used in order to generate an `OAuthToken`.
-}
type OAuthToken
    = OAuthToken String


{-| -}
oauthToken : String -> OAuthToken
oauthToken =
    OAuthToken


{-| -}
oauthTokenToString : OAuthToken -> String
oauthTokenToString (OAuthToken token) =
    token


{-| A SHA that's used as a pointer for a tree
-}
type TreeSha
    = TreeSha Never


{-| A SHA that's used as a pointer for a commit
-}
type CommitSha
    = CommitSha Never


{-| A SHA identifier
-}
type ShaHash a
    = ShaHash String


{-| -}
sha : String -> ShaHash a
sha =
    ShaHash


{-| Get the raw sha string.
-}
shaToString : ShaHash a -> String
shaToString (ShaHash shaHash) =
    shaHash


decodeSha : Json.Decode.Decoder (ShaHash a)
decodeSha =
    Json.Decode.string |> Json.Decode.map sha


encodeSha : ShaHash a -> Json.Encode.Value
encodeSha =
    shaToString >> Json.Encode.string


{-| See <https://docs.github.com/en/rest/reference/repos#create-a-fork>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createFork :
    { authToken : OAuthToken
    , owner : Owner
    , repo : String
    }
    ->
        Task
            Http.Error
            { owner : Owner
            , repo : String
            }
createFork params =
    let
        decoder =
            Json.Decode.map2
                (\owner_ repo ->
                    { owner = owner owner_
                    , repo = repo
                    }
                )
                (Json.Decode.at [ "owner", "login" ] Json.Decode.string)
                (Json.Decode.at [ "name" ] Json.Decode.string)
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/forks"
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


authorizationHeader : OAuthToken -> Http.Header
authorizationHeader (OAuthToken authToken_) =
    Http.header "Authorization" ("token " ++ authToken_)


jsonResolver : Json.Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Json.Decode.decodeString decoder body
                        |> Result.mapError Json.Decode.errorToString
                        |> Result.mapError Http.BadBody

                Http.BadUrl_ message ->
                    Err (Http.BadUrl message)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)


bytesResolver : Http.Resolver Http.Error Bytes
bytesResolver =
    Http.bytesResolver <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Ok body

                Http.BadUrl_ message ->
                    Err (Http.BadUrl message)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)



---- New stuff ----


{-| Github application client id
-}
type ClientId
    = ClientId String


{-| -}
clientId : String -> ClientId
clientId =
    ClientId


{-| -}
clientIdToString : ClientId -> String
clientIdToString (ClientId a) =
    a


{-| Github application client secret (do not include this on your frontend!)
-}
type ClientSecret
    = ClientSecret String


{-| -}
clientSecret : String -> ClientSecret
clientSecret =
    ClientSecret


{-| -}
clientSecretToString : ClientSecret -> String
clientSecretToString (ClientSecret a) =
    a


{-| Not to be confused with `OAuthToken`! This is an intermediate value you get while generating an `OAuthToken`.
-}
type OAuthCode
    = OAuthCode String


{-| -}
oauthCode : String -> OAuthCode
oauthCode =
    OAuthCode


{-| -}
oauthCodeToString : OAuthCode -> String
oauthCodeToString (OAuthCode a) =
    a


githubDomain : String
githubDomain =
    "https://github.com"


githubApiDomain : String
githubApiDomain =
    "https://api.github.com"


{-| See <https://docs.github.com/en/developers/apps/building-oauth-apps/scopes-for-oauth-apps>
-}
type Scope
    = RepoScope
    | RepoStatusScope
    | RepoDeploymentScope
    | PublicRepoScope
    | RepoInviteScope


{-| -}
scopeToString : Scope -> String
scopeToString scope =
    case scope of
        RepoScope ->
            "repo"

        RepoStatusScope ->
            "repo:status"

        RepoDeploymentScope ->
            "repo_deployment"

        PublicRepoScope ->
            "public_repo"

        RepoInviteScope ->
            "repo:invite"


{-| -}
scopeFromString : String -> Maybe Scope
scopeFromString string =
    case string of
        "repo" ->
            Just RepoScope

        "repo:status" ->
            Just RepoStatusScope

        "repo_deployment" ->
            Just RepoDeploymentScope

        "public_repo" ->
            Just PublicRepoScope

        "repo:invite" ->
            Just RepoInviteScope

        _ ->
            Nothing


{-| The link a user clicks on to be prompted about authorizing a github app.
See <https://docs.github.com/en/developers/apps/building-github-apps/identifying-and-authorizing-users-for-github-apps#1-request-a-users-github-identity>
-}
oauthLink : { clientId : ClientId, redirectUri : Maybe String, scopes : List Scope, state : Maybe String } -> String
oauthLink params =
    Url.Builder.crossOrigin
        githubDomain
        [ "login", "oauth", "authorize" ]
        (Url.Builder.string "client_id" (clientIdToString params.clientId)
            :: Url.Builder.string
                "scope"
                (List.map scopeToString params.scopes |> String.join " ")
            :: (case params.state of
                    Just state ->
                        [ Url.Builder.string "state" state ]

                    Nothing ->
                        []
               )
            ++ (case params.redirectUri of
                    Just redirectUri ->
                        [ Url.Builder.string "redirect_uri" redirectUri ]

                    Nothing ->
                        []
               )
        )


{-| See <https://docs.github.com/en/developers/apps/building-github-apps/identifying-and-authorizing-users-for-github-apps#2-users-are-redirected-back-to-your-site-by-github>
-}
getAccessToken :
    { clientId : ClientId
    , clientSecret : ClientSecret
    , oauthCode : OAuthCode
    , state : Maybe String
    }
    -> Task Http.Error AccessTokenResponse
getAccessToken params =
    Http.task
        { method = "POST"
        , headers = []
        , url =
            Url.Builder.crossOrigin
                githubDomain
                [ "login", "oauth", "access_token" ]
                (Url.Builder.string "client_id" (clientIdToString params.clientId)
                    :: Url.Builder.string "client_secret" (clientSecretToString params.clientSecret)
                    :: Url.Builder.string "code" (oauthCodeToString params.oauthCode)
                    :: (case params.state of
                            Just state ->
                                [ Url.Builder.string "state" state ]

                            Nothing ->
                                []
                       )
                )
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver <|
                \response ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            let
                                parameters =
                                    String.split "&" body
                                        |> List.filterMap
                                            (\parameter ->
                                                case String.split "=" parameter of
                                                    name :: value :: [] ->
                                                        Just ( name, value )

                                                    _ ->
                                                        Nothing
                                            )
                                        |> Dict.fromList

                                result =
                                    Maybe.map3 AccessTokenResponse
                                        (Dict.get "access_token" parameters |> Maybe.map oauthToken)
                                        (Dict.get "scope" parameters)
                                        (Dict.get "token_type" parameters)
                            in
                            case result of
                                Just good ->
                                    Ok good

                                Nothing ->
                                    ("Failed to parse parameters from body: " ++ body)
                                        |> Http.BadBody
                                        |> Err

                        Http.BadUrl_ message ->
                            Err (Http.BadUrl message)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err (Http.BadStatus metadata.statusCode)
        , timeout = Nothing
        }


{-| -}
type alias AccessTokenResponse =
    { accessToken : OAuthToken
    , scope : String
    , tokenType : String
    }


{-| See <https://docs.github.com/en/rest/reference/issues#create-an-issue>
-}
createIssue : { authToken : OAuthToken, owner : Owner, repo : String, title : NonemptyString, body : String } -> Task Http.Error ()
createIssue params =
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url =
            Url.Builder.crossOrigin githubApiDomain
                [ "repos", ownerToString params.owner, params.repo, "issues" ]
                []
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "title", Json.Encode.string (String.Nonempty.toString params.title) )
                    , ( "body", Json.Encode.string params.body )
                    ]
                )
        , resolver = jsonResolver (Json.Decode.succeed ())
        , timeout = Nothing
        }
