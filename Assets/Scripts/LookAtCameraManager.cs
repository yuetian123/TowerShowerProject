using UnityEngine;

public class LookAtCameraManager : IEffectManager<LookAtCameraManager, LookAtCamera>
{
    Transform _maniCameraTransform;

    void Update()
    {
        if (_maniCameraTransform == null)
        {
            _maniCameraTransform = Camera.main.transform;
        }

        if (_maniCameraTransform != null)
        {
            var camPos = _maniCameraTransform.position;
            for (var i = 0; i < LookAtCameraList.Count; i++)
            {
                var it = LookAtCameraList[i];
                if (it == null)
                {
                    LookAtCameraList.Remove(it);
                    i--;
                    continue;
                }

                it.transform.LookAt(camPos, -Vector3.up);
            }
        }
    }
}