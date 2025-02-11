# Changelog

## 0.1.6.0

- Support base64-1.0

## 0.1.5.0

- Convert userDisplayName into a Maybe value [#30](https://github.com/softwarefactory-project/matrix-client-haskell/issues/30)
- Improve empty response handling [#32](https://github.com/softwarefactory-project/matrix-client-haskell/issues/32)

## 0.1.4.3

- Add missing export for Dir.

## 0.1.4.2

- Support retry-0.9

## 0.1.4.1

- Support aeson-2.0

## 0.1.4.0

- Completes The Room API

## 0.1.3.0

- Adds Lenses and Prisms
- Adds login/logout functiosn for generating and destroying Matrix Tokens
- Add functionality to set and retrieve non-room account data
- Generalize retry to arbitrary MatrixM

## 0.1.2.0

- Add filtering client function
- Add sync client function
- Add createRoom client function
- Add retryWithLog and syncPoll utility function
- Add mkReply helper utility function
- Add reply and edit Event
- Change MessageText to include the TextType
- Change RoomEvent to use Author and EventID newtype

## 0.1.1.0

- Ensure aeson encoding test is reproducible using aeson-pretty
- Increase retry delay up to 2 minutes
- Add leaveRoomById client function
- Add joinRoom client function
- Handle 400s error message returned by the API
- Handle rate limit response in the retry helper

## 0.1.0.0

- Initial release
