/**
  *  analysis.als
  *    A module that puts everything together
  *    End-to-end analysis of security properties
  */
module analysis

open flow


// General security properties
sig TrustedModule, MaliciousModule in FlowModule {}
sig PrivateData, MaliciousData in Data {}

// No malicious module should be able to access private data
assert Confidentiality {
  no m: MaliciousModule, t: Time | 
    some PrivateData & m.accesses.t
}

// No malicious data should ever flow into a trusted module
assert Integrity {
  no u: TrustedModule, t: Time | 
    some MaliciousData & u.accesses.t
}

check Confidentiality for 3
check Integrity for 3


/* Restrictions */

-- you can comment out any of these facts if you prefer


// Remove trivial cases 
fact  {
  -- no malicious module begins with private data
  no m: MaliciousModule | some PrivateData & m.accesses.first
  -- no trusted module begins with malicious data
  no m: TrustedModule | some MaliciousData & m.accesses.first
  -- no module is both trusted and malicious
  no TrustedModule & MaliciousModule
  -- no data is both private and malicious
  no PrivateData & MaliciousData
}

// Restrict calls to be only of one kind
-- leave uncommented the kind of call you want to see only
fact {
  -- all c: Call | c in SetDomain
  all c: Call | c in XmlHttpRequest
}

/* Helper functions for visualization */

fun currentCall: Call -> Time {
  {c: Call, t: Time | c.start = t }
}
