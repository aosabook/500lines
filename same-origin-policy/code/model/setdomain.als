/**
  *  setdomain.als
  *    A model of the operations related to the document.domain
  *    property
  */
module setDomain
open script

// Modify the document.domain property
sig SetDomain extends BrowserOp { newDomain: Domain }{
  doc = from.context
  domain.end = domain.start ++ doc -> newDomain
  -- no change to the content of the document
  content.end = content.start
}

// Scripts can only set the domain property to only one that is a right-hand,
// fully-qualified fragment of its hostname
fact setDomainRule {
   all d: Document | d.src.host in (d.domain.Time).subsumes
}

// Can a script set the "document.domain" property with a new_domain that
// doesn't match the host of the src?
run { some sd: SetDomain | sd.doc.src.host != sd.newDomain }

// Can a script set the "document.domain" property with a new_domain that
// doesn't match the host of the src even if there are no domains that subsume
// others?
run {
  some SetDomain
  all sd: SetDomain | sd.doc.src.host != sd.newDomain
  no d: Domain | some d.subsumes
}
