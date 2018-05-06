clear all
clc
list_all = ls('*.tif');
for iii = 1:size(list_all,1)
    [dummie,R]= geotiffread(list_all(iii,:));
    for i = 1:size(dummie,1)
        for j = 1:size(dummie,2)
            for k = 2:size(dummie,3)
                if (dummie(i,j,k-1)>0)&&(dummie(i,j,k)==1)
                    dummie(i,j,k) = dummie(i,j,k-1) + dummie(i,j,k);
                end
            end
        end
    end
    filename = strcat(list_all(iii,1:end-4),'_CS.tif');
    geotiffwrite(filename,dummie,R);
    [max_duration,time_stamp] = max(dummie,[],3);
    max_duration = double(max_duration);
    time_stamp_end = double(time_stamp);
    max_duration(max_duration == 0) = NaN;
    time_stamp_end(isnan(max_duration)) = NaN;
    time_stamp_start = time_stamp_end - max_duration;
    max_duration = max_duration/4;
    dummie = cat(3,max_duration,time_stamp_start,time_stamp_end);
    filename_max_hours = strcat(list_all(iii,1:end-4),...
        '_Max_duration_in_hours.tif');
    geotiffwrite(filename_max_hours,dummie,R);
end