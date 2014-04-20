module event [T]

sig Time {}
sig Event  extends T {pre, post: Time}
