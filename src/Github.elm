module Github exposing
    ( getBranch, createBranch
    , getCommit, createCommit
    , PullRequest, getPullRequests, getPullRequest, createPullRequest
    , getFileContents, updateFileContents
    , createBlob, getBlobAsBase64
    , getComments, createComment
    )

{-|

@docs getBranch, createBranch
@docs getCommit, createCommit
@docs PullRequest, getPullRequests, getPullRequest, createPullRequest
@docs getFileContents, updateFileContents
@docs createBlob, getBlobAsBase64


## Issues

@docs getComments, createComment

-}

import Base64
import Http
import Iso8601
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Time


{-| See <https://developer.github.com/v3/git/commits/#get-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getCommit :
    { authToken : String
    , repo : String
    , sha : String
    }
    ->
        Task Http.Error
            { sha : String
            , tree :
                { sha : String
                }
            }
getCommit params =
    let
        decoder =
            Json.Decode.map2
                (\sha treeSha ->
                    { sha = sha
                    , tree = { sha = treeSha }
                    }
                )
                (Json.Decode.at [ "sha" ] Json.Decode.string)
                (Json.Decode.at [ "tree", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/commits/" ++ params.sha
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/git/commits/#create-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createCommit :
    { authToken : String
    , repo : String
    , message : String
    , tree : String
    , parents : List String
    }
    ->
        Task Http.Error
            { sha : String
            }
createCommit params =
    let
        decoder =
            Json.Decode.at [ "sha" ] Json.Decode.string
                |> Json.Decode.map (\sha -> { sha = sha })
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/commits"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "tree", Json.Encode.string params.tree )
                    , ( "parents", Json.Encode.list Json.Encode.string params.parents )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/git/refs/#get-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getBranch :
    { authToken : String
    , repo : String
    , branchName : String
    }
    ->
        Task Http.Error
            { object :
                { sha : String
                }
            }
getBranch params =
    let
        decoder =
            Json.Decode.at [ "object", "sha" ] Json.Decode.string
                |> Json.Decode.map (\sha -> { object = { sha = sha } })
    in
    Http.task
        { method = "GET"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/refs/heads/" ++ params.branchName
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/git/refs/#create-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createBranch :
    { authToken : String
    , repo : String
    , branchName : String
    , sha : String
    }
    -> Task Http.Error ()
createBranch params =
    let
        decoder =
            Json.Decode.succeed ()
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/refs"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "ref", Json.Encode.string ("refs/heads/" ++ params.branchName) )
                    , ( "sha", Json.Encode.string params.sha )
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
    { authToken : String
    , repo : String
    }
    -> Task Http.Error (List PullRequest)
getPullRequests params =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decodePullRequest)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getPullRequest :
    { authToken : String
    , repo : String
    , number : Int
    }
    ->
        Task Http.Error
            { head :
                { ref : String
                , sha : String
                }
            }
getPullRequest params =
    let
        decoder =
            Json.Decode.map2
                (\headRef headSha ->
                    { head =
                        { ref = headRef
                        , sha = headSha
                        }
                    }
                )
                (Json.Decode.at [ "head", "ref" ] Json.Decode.string)
                (Json.Decode.at [ "head", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls/" ++ String.fromInt params.number
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#create-a-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createPullRequest :
    { authToken : String
    , repo : String
    , branchName : String
    , baseBranch : String
    , title : String
    , description : String
    }
    -> Task Http.Error ()
createPullRequest params =
    let
        decoder =
            Json.Decode.succeed ()
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "title", Json.Encode.string params.title )
                    , ( "head", Json.Encode.string params.branchName )
                    , ( "base", Json.Encode.string params.baseBranch )
                    , ( "body", Json.Encode.string params.description )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/repos/contents/#get-contents>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getFileContents :
    { authToken : String
    , repo : String
    , ref : String
    , path : String
    }
    ->
        Task Http.Error
            { encoding : String
            , content : String
            , sha : String
            }
getFileContents params =
    let
        decoder =
            Json.Decode.map3
                (\encoding content sha ->
                    { encoding = encoding
                    , content = content
                    , sha = sha
                    }
                )
                (Json.Decode.at [ "encoding" ] Json.Decode.string)
                (Json.Decode.at [ "content" ] Json.Decode.string)
                (Json.Decode.at [ "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path ++ "?ref=" ++ params.ref
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/repos/contents/#update-a-file>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
updateFileContents :
    { authToken : String
    , repo : String
    , branch : String
    , path : String
    , sha : String
    , message : String
    , content : String
    }
    ->
        Task Http.Error
            { content :
                { sha : String
                }
            }
updateFileContents params =
    let
        decoder =
            Json.Decode.map
                (\contentSha ->
                    { content = { sha = contentSha } }
                )
                (Json.Decode.at [ "content", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "content", Json.Encode.string (Base64.encode params.content) )
                    , ( "sha", Json.Encode.string params.sha )
                    , ( "branch", Json.Encode.string params.branch )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getComments :
    { authToken : String
    , repo : String
    , issueNumber : Int
    }
    ->
        Task Http.Error
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
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/issues/" ++ String.fromInt params.issueNumber ++ "/comments"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decoder)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#create-a-comment>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createComment :
    { authToken : String
    , repo : String
    , issueNumber : Int
    , body : String
    }
    ->
        Task Http.Error
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
        , headers = [ Http.header "Authorization" ("token " ++ params.authToken) ]
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


{-| See <https://docs.github.com/en/rest/reference/git#get-a-blob>

This function returns the blob content as a base64-encoded string.

NOTE: Not all output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getBlobAsBase64 :
    { repo : String
    , fileSha : String
    }
    ->
        Task Http.Error
            { content : String
            }
getBlobAsBase64 params =
    let
        decoder =
            Json.Decode.map
                (\content ->
                    { content = content
                    }
                )
                (Json.Decode.at [ "content" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/vnd.github.v3+json"
            ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/blobs" ++ "/" ++ params.fileSha
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#create-a-blob>

NOTE: Not all output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createBlob :
    { authToken : String
    , repo : String
    , content : String
    }
    ->
        Task Http.Error
            { sha : String
            }
createBlob params =
    let
        decoder =
            Json.Decode.at [ "sha" ] Json.Decode.string
                |> Json.Decode.map (\sha -> { sha = sha })
    in
    Http.task
        { method = "POST"
        , headers =
            [ Http.header "Authorization" ("token " ++ params.authToken)
            , Http.header "Accept" "application/vnd.github.v3+json"
            ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/blobs"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "content", Json.Encode.string params.content )
                    , ( "encoding", Json.Encode.string "utf-8" )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }
