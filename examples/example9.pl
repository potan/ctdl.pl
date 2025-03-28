
% Eventuality types and thematic roles

ct_Eventuality(soaLeave).
ct_Eventuality(soaPay).

ct_ThematicRole(soaHas_agent).
ct_ThematicRole(soaHas_object).
ct_ThematicRole(soaHas_instrument).


% Input facts
ct_triple(soa_elj, type, optional).
ct_triple(soa_elj, type, soaLeave).
ct_triple(soa_elj, not, soa_enlj).  % It is required...
% ThematicRole must not present...

ct_triple(soa_npelj, type, false).
ct_triple(soa_npelj, type, hold).
meta_triple(soa_npelj, soa_elj, type, permitted).
