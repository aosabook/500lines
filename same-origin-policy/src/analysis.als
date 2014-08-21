/**
  *  analysis.als
  *    A module that puts everything together
  *    End-to-end analysis of security properties
  */
module analysis

open flow


// General security properties
sig TrustedModule, MaliciousModule in FlowModule {}
sig CriticalData, MaliciousData in Data {}

// No malicious module should be able to access private data
assert Confidentiality {
  no m : FlowModule - TrustedModule, t : Time |
    some CriticalData & m.accesses.t 
}

// No malicious data should ever flow into a trusted module
assert Integrity {
  no u: TrustedModule, t: Time | 
    some MaliciousData & u.accesses.t
}

fact  {
  -- no malicious module begins with private data
  no m: MaliciousModule | some CriticalData & m.accesses.first
  -- no trusted module begins with malicious data
  no m: TrustedModule | some MaliciousData & m.accesses.first
  -- no module is both trusted and malicious
  no TrustedModule & MaliciousModule
  -- no data is both private and malicious
  no CriticalData & MaliciousData
}

check Confidentiality for 3
check Integrity for 3


/* Restrictions */

-- you can comment out any of these facts if you prefer

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
