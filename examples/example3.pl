
% Eventuality types and thematic roles

ct_Eventuality(soaLeave).
ct_Eventuality(soaEat).
ct_Eventuality(soaDrink).

ct_ThematicRole(soaHas_agent).


% Input facts
ct_triple(soa_eo, type, rexist).
ct_or(soa_eo, [soa_elj, soa_ea]).
ct_and(soa_ea, [soa_eej, soa_edj]).

ct_triple(soa_elj, type, soa_Leave).
ct_triple(soa_elj, soaHas_agent, soa_John).

ct_triple(soa_eej, type, soa_Eat).
ct_triple(soa_eej, soaHas_agent, soa_John).

ct_triple(soa_edj, type, soa_Drink).
ct_triple(soa_edj, soaHas_agent, soa_John).

% John does not leave
ct_triple(soa_enlj, not, soa_elj).
ct_triple(soa_enlj, type, rexist).
